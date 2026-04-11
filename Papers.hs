{-# LANGUAGE DeriveAnyClass, DeriveGeneric, ImportQualifiedPost, LambdaCase, OverloadedStrings, ScopedTypeVariables #-}
module Papers where

import Control.Arrow ((>>>))
import Control.Monad ((>=>), foldM, forM, forM_, guard, unless, void, when)
import Control.Monad.State (State, get, lift, runState, put)
import Data.Aeson (FromJSON, eitherDecode, eitherDecodeFileStrict, fromJSON, parseJSON, withArray, withObject, (.:))
import Data.Aeson.Types (Object, Parser, Value, (.:?))
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as ByteString
import Data.Char (isNumber, toLower)
import Data.Foldable (toList)
import Data.Function ((&), on)
import Data.List (intercalate, sort, stripPrefix)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust, isJust)
import Data.String (fromString)
import Data.Text.Lazy (Text, unpack)
import Data.Time (Day, NominalDiffTime, TimeOfDay)
import Data.Time qualified as Time
import Data.Time.Format.ISO8601 qualified as ISO8601
import Data.Tuple (swap)
import GHC.Generics (Generic)
import System.Directory (listDirectory)
import System.FilePath (takeBaseName, takeExtension)

import Text.Blaze.Html.Renderer.Pretty qualified as BlazePretty
import Text.Blaze.Html.Renderer.String qualified as BlazeString
import Text.Blaze.Html5 (Html)
import Text.Blaze.Html5 qualified as Blaze
import Text.Blaze.Html5.Attributes qualified as BlazeAttr

-- Utilities

-- strip_suffix :: (Eq a) => [a] -> [a] -> Maybe [a]
-- strip_suffix suffix = reverse >>> stripPrefix (reverse suffix) >>> fmap reverse

fail_maybe :: (MonadFail m) => String -> Maybe a -> m a
fail_maybe msg = maybe (fail msg) return

maybeM_ :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
maybeM_ x f = maybe (return ()) f x

parens :: String -> String
parens s = "(" ++ s ++ ")"

time_add :: NominalDiffTime -> TimeOfDay -> TimeOfDay
time_add diff = Time.daysAndTimeOfDayToTime 0
  >>> (+ diff)
  >>> (snd . Time.timeToDaysAndTimeOfDay)

-- JSON parsing.

decode_json :: (FromJSON a, MonadFail m) => ByteString -> m a
decode_json bs = case eitherDecode bs of
  Left msg -> fail msg
  Right x -> return x

to_map :: (MonadFail m, Ord k, Show k) => String -> String -> [(k, v)] -> m (Map k v)
to_map context key_name = foldM f Map.empty where
  f r (k, v) = case Map.insertLookupWithKey (\k n o -> n) k v r of
    (Nothing, r) -> return r
    _ -> fail $ context ++ ": duplicate " ++ key_name ++ " " ++ show k

-- Reading the JSON invited talks file.

data Invited = Invited
  { invited_speaker :: String
  , invited_affiliation :: String
  , invited_homepage :: Maybe String
  , invited_title :: Maybe String
  , invited_abstract :: Maybe String
  , invited_abstract_html :: Maybe String
  , invited_picture :: Maybe String
  } deriving (Eq, Show)

instance FromJSON Invited where
  parseJSON = withObject "invited" $ \v -> Invited
    <$> v .: "speaker"
    <*> v .: "affiliation"
    <*> v .:? "homepage"
    <*> v .:? "title"
    <*> v .:? "abstract"
    <*> v .:? "abstract_html"
    <*> return Nothing

type Inviteds = Map String Invited

parse_file_inviteds :: FilePath -> IO Inviteds
parse_file_inviteds = ByteString.readFile >=> decode_json

picture_from_path :: FilePath -> Maybe String
picture_from_path path = do
  guard $ takeExtension path `elem` [".jpg", ".png", ".webm"]
  return $ takeBaseName path

parse_picture :: (MonadFail m) => FilePath -> m String
parse_picture path = fail_maybe ("invalid picture filename: " ++ path) $ picture_from_path path

type InvitedPictures = Map String FilePath

inviteds_with_pictures :: InvitedPictures -> Inviteds -> Inviteds
inviteds_with_pictures pictures = Map.mapWithKey $
    \id paper -> paper { invited_picture = Map.lookup id pictures }

-- Reading the JSON sessions file.

data Session = Session
  { session_id :: Integer
  , session_title :: String
  , session_papers :: [Integer]
  } deriving (Eq, Show)

instance FromJSON Session where
  parseJSON = withObject "session" $ \v -> Session
    <$> v .: "id"
    <*> v .: "title"
    <*> v .: "papers"

type Sessions = Map Integer Session

parse_file_sessions :: FilePath -> IO Sessions
parse_file_sessions = ByteString.readFile
  >=> decode_json
  >>> fmap (map (\s -> (session_id s, s)))
  >=> to_map "sessions" "id"

-- Reading the JSON schedule file.

type TimeOfDayRange = (TimeOfDay, TimeOfDay)

data Title = Title
  { title_string :: String
  , title_html :: Maybe String
  } deriving (Eq, Show)

parse_title :: Object -> Parser Title
parse_title v = Title
  <$> v .: "title"
  <*> v .:? "title_html"

data Event = EventBreak { event_break :: Title }
           | EventInvitedTalk { event_invited_talk_key :: String }
           | EventSession { event_session_id :: Integer }
           | EventSpecial { event_special :: Title }
  deriving (Eq, Show)

time_format :: ISO8601.Format TimeOfDay
time_format = ISO8601.hourMinuteFormat ISO8601.ExtendedFormat

time_show :: TimeOfDay -> String
time_show = ISO8601.formatShow time_format

time_parse :: (MonadFail m) => String -> m TimeOfDay
time_parse = ISO8601.formatParseM time_format

parse_ranged_event :: Value -> Parser (TimeOfDayRange, Event)
parse_ranged_event = withObject "ranged_event" $ \v -> do
  _from <- v .: "from" >>= time_parse
  _to <- v .: "to" >>= time_parse
  _type :: Text <- v .: "type"
  event <- case _type of
    "break" -> parse_title v & fmap EventBreak
    "invited_talk" -> v .: "speaker" & fmap EventInvitedTalk
    "session" -> v .: "id" & fmap EventSession
    "special" -> parse_title v & fmap EventSpecial
    _ -> fail $ "unknown event type " ++ unpack _type
  return ((_from, _to), event)

type DaySchedule = [(TimeOfDayRange, Event)]

parse_ranged_events :: Day -> Value -> Parser DaySchedule
parse_ranged_events date = withArray "ranged_events" $ \a -> do
  ranged_events <- traverse parse_ranged_event $ toList a
  forM_ ranged_events check_duration
  case ranged_events of
    [] -> return ()
    x : xs -> forM_ (map swap $ zip xs ranged_events) check_overlap
  return ranged_events
  where
  check_duration ((start, end), _) = unless (start <= end) $ fail $
    "event on " ++ show date ++ " with non-positive duration: "
    ++ "start " ++ time_show start ++ ", "
    ++ "end " ++ time_show end
  check_overlap (prev@((_, prev_end), _), next@((next_start, _), _)) =
    unless (prev_end <= next_start) $ fail $
      "events on " ++ show date ++ " overlap: "
      ++ "previous end " ++ time_show prev_end ++ ", "
      ++ "next start " ++ time_show next_start

parse_day :: Value -> Parser (Day, DaySchedule)
parse_day = withObject "Day" $ \v -> do
  _date <- v .: "date" >>= ISO8601.iso8601ParseM
  ranged_events <- v .: "events" >>= parse_ranged_events _date
  return (_date, ranged_events)

newtype Schedule = Schedule { schedule :: Map Day DaySchedule }
  deriving (Eq, Show)

instance FromJSON Schedule where
  parseJSON = withArray "schedule" $
    toList >>> traverse parse_day >=> to_map "schedule" "day" >>> fmap Schedule

parse_file_schedule :: FilePath -> IO Schedule
parse_file_schedule = ByteString.readFile >=> decode_json

-- Reading the JSON papers file.

data Author = Author
  { author_first :: String
  , author_last :: String
  , author_affiliation :: String
  } deriving (Eq, FromJSON, Generic, Show)

lowercase :: String -> String
lowercase = map toLower

instance Ord Author where
  compare = compare `on` sequence [author_last, author_first] >>> map lowercase

data Paper = Paper
  { paper_id :: Integer
  , paper_title :: String
  , paper_authors :: [Author]
  , paper_path :: Maybe FilePath
  } deriving (Eq, Show)

instance Ord Paper where
  compare = compare `on` paper_authors

instance FromJSON Paper where
  parseJSON = withObject "Paper" $ \v -> do
    _type :: Text <- v .: "object"
    unless (_type == "paper") $ fail "not a paper"
    Paper
      <$> v .: "pid"
      <*> v .: "title"
      <*> v .: "authors"
      <*> return Nothing

type Papers = Map Integer Paper

parse_file_papers :: FilePath -> IO Papers
parse_file_papers = ByteString.readFile
  >=> decode_json
  >>> fmap (map (\s -> (paper_id s, s)))
  >=> to_map "papers" "id"

type PaperAbstracts = Map Integer FilePath

abstract_id_from_path :: FilePath -> Maybe Integer
abstract_id_from_path path = do
  let s = takeBaseName path
  guard $ all isNumber s
  return $ read s

parse_abstract :: (MonadFail m) => FilePath -> m Integer
parse_abstract path = fail_maybe ("invalid abstract filename: " ++ path) $ abstract_id_from_path path

-- parse_abstracts :: FilePath -> IO Abstracts
-- parse_abstracts = listDirectory
--   >=> traverse parse_abstract
--   >>> fmap Map.fromList

papers_with_abstract :: PaperAbstracts -> Papers -> Papers
papers_with_abstract abstracts = Map.mapWithKey $
    \id paper -> paper { paper_path = Map.lookup id abstracts}

-- parse_papers :: FilePath -> FilePath -> IO Papers
-- parse_papers path_json path_abstracts = do
--   papers <- parse_file_papers path_json
--   abstracts <- parse_abstracts path_json
--   return $ papers_with_abstract abstracts papers

-- Data.

talk_length :: NominalDiffTime
talk_length = Time.daysAndTimeOfDayToTime 0 $ Time.midnight {Time.todMin = 20}


-- HTML rendering.

-- Needed for preserving whitespace.
blaze_strict :: Html -> Html
blaze_strict = BlazeString.renderHtml >>> Blaze.preEscapedString

blaze_li_strict :: Html -> Html
blaze_li_strict = Blaze.li >>> blaze_strict

blaze_ul :: [Html] -> Html
blaze_ul = map Blaze.li >>> mconcat >>> Blaze.ul

blaze_ul_strict :: [Html] -> Html
blaze_ul_strict = map (Blaze.li >>> blaze_strict) >>> mconcat >>> Blaze.ul

blaze_link :: String -> Html -> Html
blaze_link target = Blaze.a Blaze.! BlazeAttr.href (fromString target)

blaze_link_maybe :: Maybe String -> Html -> Html
blaze_link_maybe = maybe id blaze_link

blaze_link_else_italic :: Maybe String -> Html -> Html
blaze_link_else_italic = \case
  Nothing -> Blaze.i
  Just target -> blaze_link target

anchorize :: String -> String -> String
anchorize url anchor = url ++ "#" ++ anchor

-- Papers.

format_author :: Author -> String
format_author = sequence [first, last] >>> intercalate " "

format_authors :: Paper -> String
format_authors = paper_authors >>> map format_author >>>  intercalate ", "

format_paper :: Paper -> Html
format_paper paper = do
  Blaze.string $ format_authors paper
  Blaze.string ": "
  blaze_link_else_italic (paper_path paper) (Blaze.string $ paper_title paper)

format_papers :: Papers -> String
format_papers = toList
  >>> sort
  >>> map format_paper
  >>> blaze_ul_strict
  >>> BlazePretty.renderHtml

-- Invited speakers.

invited_speaker_picture :: String -> String
invited_speaker_picture = ("images/invited/" ++)

invited_speaker_link :: String -> String
invited_speaker_link = anchorize "invited-speakers.html"

format_invited_speaker :: String -> Invited -> Html
format_invited_speaker key invited = Blaze.div
  Blaze.! BlazeAttr.id (fromString key)
  Blaze.! BlazeAttr.class_ "row border rounded m-1" $ do
    Blaze.div Blaze.! BlazeAttr.class_ "col-md-auto m-2" $
      case invited_picture invited of
        Nothing -> Blaze.div Blaze.! BlazeAttr.style "width: 200px;" $ return ()
        Just picture -> Blaze.img
          Blaze.! BlazeAttr.src (fromString picture)
          Blaze.! BlazeAttr.alt (fromString $ invited_speaker invited)
          Blaze.! BlazeAttr.class_ "img-fluid"
          Blaze.! BlazeAttr.width "200px"
    Blaze.div Blaze.! BlazeAttr.class_ "col m-2" $ do
      blaze_strict $ black $ Blaze.h4 $ do
        blaze_link_maybe (invited_homepage invited) $ Blaze.string $ invited_speaker invited
        Blaze.string $ " " ++ parens (invited_affiliation invited)
      maybeM_ (invited_title invited) $ Blaze.string >>> Blaze.h5 >>> black
      when (any (($ invited) >>> isJust) [invited_abstract_html, invited_abstract]) $ do
        Blaze.details Blaze.! BlazeAttr.open mempty $ do
          Blaze.summary $ Blaze.string "Abstract"
          case invited_abstract_html invited of
            Just s -> Blaze.preEscapedString s
            Nothing -> Blaze.p $ Blaze.string $ fromJust $ invited_abstract invited
  where
  -- HACK: undo styling of headers.
  black :: Html -> Html
  black = (Blaze.! BlazeAttr.style "color: black;")

format_invited_speakers :: Inviteds -> String
format_invited_speakers = Map.toAscList
  >>> map (uncurry format_invited_speaker)
  >>> mconcat
  >>> Blaze.div Blaze.! BlazeAttr.class_ "vstack gap-3 mb-4"
  >>> BlazePretty.renderHtml

-- Schedule

show_id :: Integer -> String
show_id = (+1) >>> show

format_time_range :: TimeOfDayRange -> String
format_time_range (start, end) = time_show start ++ "–" ++ time_show end

format_title :: Title -> Html
format_title title = case title_html title of
  Nothing -> Blaze.string $ title_string title
  Just v -> Blaze.preEscapedString v

format_schedule :: Papers -> Inviteds -> Sessions -> Schedule -> String
format_schedule papers inviteds sessions = schedule
  >>> Map.traverseWithKey format_day_schedule >>> void
  >>> BlazePretty.renderHtml
  where
  format_day_schedule :: Day -> DaySchedule -> Html
  format_day_schedule date ranged_events = do
    Blaze.h3 $ Blaze.string $ show $ Time.dayOfWeek date
    Blaze.ul $ mconcat $ map format_ranged_event ranged_events

  format_ranged_event :: (TimeOfDayRange, Event) -> Html
  format_ranged_event (range@(start, end), event) = do
    case event of
      EventBreak title -> inline $ format_title title
      EventInvitedTalk key -> do
        inline $ format_invited_talk key (inviteds Map.! key)
      EventSession id -> format_session (sessions Map.! id)
      EventSpecial title -> inline $ format_title title
    where
    prefix :: Html
    prefix = do
      Blaze.b $ Blaze.string $ format_time_range range ++ ":"
      Blaze.string " "

    inline :: Html -> Html
    inline x = blaze_li_strict $ prefix <> x

    format_invited_talk :: String -> Invited -> Html
    format_invited_talk key invited = case invited_title invited of
      Nothing -> blaze_link (invited_speaker_link key) $ Blaze.string $ invited_speaker invited
      Just title -> do
        Blaze.string (invited_speaker invited)
        Blaze.string ": "
        blaze_link (invited_speaker_link key) $ Blaze.string title

    format_talk :: Paper -> State TimeOfDay Html
    format_talk paper = do
      start <- get
      let end = time_add talk_length start
      put end
      return $ do
        Blaze.string $ format_time_range (start, end)
        Blaze.string ": "
        format_paper paper

    format_session :: Session -> Html
    format_session session =
      Blaze.li $ do
        blaze_strict $ prefix <> (Blaze.string $ "Session " ++ show_id id)
        blaze_ul_strict items_checked
      where
      id = session_id session
      (items, end_computed) = session & session_papers &
        traverse ((papers Map.!) >>> format_talk) &
        flip runState start
      items_checked = if end_computed == end
        then items
        else error $ "session " ++ show_id id ++ " has bad length: "
             ++ "ends at " ++ time_show end ++ ", "
             ++ "but talks end at " ++ time_show end_computed
