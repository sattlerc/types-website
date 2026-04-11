{-# LANGUAGE ImportQualifiedPost, OverloadedStrings, ScopedTypeVariables #-}
module Parse where

import Control.Arrow ((>>>))
import Control.Monad ((>=>), foldM, forM_, guard, unless)
import Data.Aeson (FromJSON, eitherDecode, parseJSON, withArray, withObject, (.:))
import Data.Aeson.Types (Object, Parser, Value, (.:?))
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as ByteString
import Data.Char (isNumber, toLower)
import Data.Foldable (toList)
import Data.Function ((&), on)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text.Lazy (Text, unpack)
import Data.Time (Day, NominalDiffTime, TimeOfDay(TimeOfDay))
import Data.Time qualified as Time
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.Format.ISO8601 qualified as ISO8601
import Data.Tuple (swap)
import System.FilePath (takeBaseName, takeExtension)

-- Utilities

-- strip_suffix :: (Eq a) => [a] -> [a] -> Maybe [a]
-- strip_suffix suffix = reverse >>> stripPrefix (reverse suffix) >>> fmap reverse

fail_maybe :: (MonadFail m) => String -> Maybe a -> m a
fail_maybe msg = maybe (fail msg) return

maybeM_ :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
maybeM_ x f = maybe (return ()) f x

parens :: String -> String
parens s = "(" ++ s ++ ")"

time_of_day_to_diff :: TimeOfDay -> NominalDiffTime
time_of_day_to_diff = Time.daysAndTimeOfDayToTime 0

diff_to_time_of_day :: NominalDiffTime -> TimeOfDay
diff_to_time_of_day = Time.timeToDaysAndTimeOfDay >>> snd

time_add :: NominalDiffTime -> TimeOfDay -> TimeOfDay
time_add diff = time_of_day_to_diff >>> (+ diff) >>> diff_to_time_of_day

time_ratio :: (Fractional a) => NominalDiffTime -> NominalDiffTime -> a
time_ratio = curry $ uncurry ((/) `on` toRational) >>> fromRational

time_scale :: (Real a) => a -> NominalDiffTime -> NominalDiffTime
time_scale s = toRational >>> (toRational s *) >>> fromRational

time_of_day_diff :: TimeOfDay -> TimeOfDay -> NominalDiffTime
time_of_day_diff = on subtract time_of_day_to_diff

time_format :: ISO8601.Format TimeOfDay
time_format = ISO8601.hourMinuteFormat ISO8601.ExtendedFormat

time_show :: TimeOfDay -> String
time_show = ISO8601.formatShow time_format

time_parse :: (MonadFail m) => String -> m TimeOfDay
time_parse = ISO8601.formatParseM time_format

show_month_and_day :: Day -> String
show_month_and_day = formatTime defaultTimeLocale "%e %B"

show_day_detailed :: Day -> String
show_day_detailed date = show (Time.dayOfWeek date) ++ ", " ++ show_month_and_day date

-- JSON parsing.

decode_json :: (FromJSON a, MonadFail m) => ByteString -> m a
decode_json bs = case eitherDecode bs of
  Left msg -> fail msg
  Right x -> return x

to_map :: (MonadFail m, Ord k, Show k) => String -> String -> [(k, v)] -> m (Map k v)
to_map context key_name = foldM f Map.empty where
  f r (k, v) = case Map.insertLookupWithKey (\_ n _ -> n) k v r of
    (Nothing, t) -> return t
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
    \id_ invited -> invited { invited_picture = Map.lookup id_ pictures }

-- Reading the JSON sessions file.

data Session = Session
  { session_id :: Integer
  , session_title :: String
  , session_papers :: [Integer]
  } deriving (Eq, Show)

instance FromJSON Session where
  parseJSON = withObject "Session" $ \v -> Session
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
  , title_short :: Maybe String
  , title_html :: Maybe String
  , title_link :: Maybe String
  } deriving (Eq, Show)

parse_title :: Object -> Parser Title
parse_title v = Title
  <$> v .: "title"
  <*> v .:? "short"
  <*> v .:? "title_html"
  <*> v .:? "link"

data Event = EventBreak { event_break :: Title }
           | EventInvitedTalk { event_invited_talk_key :: String }
           | EventSession { event_session_id :: Integer }
           | EventSpecial { event_special :: Title }
  deriving (Eq, Show)

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
    _ : xs -> forM_ (map swap $ zip xs ranged_events) check_overlap
  return ranged_events
  where
  check_duration ((start, end), _) = unless (start <= end) $ fail $
    "event on " ++ show date ++ " with non-positive duration: "
    ++ "start " ++ time_show start ++ ", "
    ++ "end " ++ time_show end
  check_overlap (((_, prev_end), _), ((next_start, _), _)) =
    unless (prev_end <= next_start) $ fail $
      "events on " ++ show date ++ " overlap: "
      ++ "previous end " ++ time_show prev_end ++ ", "
      ++ "next start " ++ time_show next_start

parse_day :: Value -> Parser (Day, DaySchedule)
parse_day = withObject "Day" $ \v -> do
  _date <- v .: "date" >>= ISO8601.iso8601ParseM
  ranged_events <- v .: "events" >>= parse_ranged_events _date
  return (_date, ranged_events)

newtype Schedule = Schedule { unschedule :: Map Day DaySchedule }
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
  } deriving (Eq, Show)

instance FromJSON Author where
  parseJSON = withObject "Author" $ \v -> Author
    <$> v .: "first"
    <*> v .: "last"
    <*> v .: "affiliation"

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
    \id_ paper -> paper { paper_path = Map.lookup id_ abstracts}

-- parse_papers :: FilePath -> FilePath -> IO Papers
-- parse_papers path_json path_abstracts = do
--   papers <- parse_file_papers path_json
--   abstracts <- parse_abstracts path_json
--   return $ papers_with_abstract abstracts papers

-- Data.

talk_length :: NominalDiffTime
talk_length = time_of_day_to_diff $ Time.midnight {Time.todMin = 20}

schedule_unit :: NominalDiffTime
schedule_unit = time_of_day_to_diff $ TimeOfDay 1 0 0
