module Parse where

import Control.Applicative ((<|>))
import Control.Arrow ((>>>), (&&&))
import Control.Monad ((>=>), foldM, forM_, guard, unless)
import Data.Aeson (FromJSON, Value(Object), eitherDecode, parseJSON, withArray, withObject, (.:))
import Data.Aeson.Types (Object, Parser, (.:?))
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as ByteString
import Data.Foldable (toList)
import Data.Function ((&), on)
import Data.Functor.Classes (liftCompare, liftCompare2)
import Data.List (intercalate, sort)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Text.Lazy (Text, unpack)
import Data.Time (Day, NominalDiffTime, TimeOfDay(TimeOfDay))
import Data.Time qualified as Time
import Data.Time.Format.ISO8601 qualified as ISO8601
import Data.Tuple (swap)
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>), splitDirectories, takeBaseName, takeExtension)

import General

-- Path parsing.

list_files_or_index_html :: FilePath -> IO [FilePath]
list_files_or_index_html dir = do
  paths <- list_directory dir
  traverse h paths where

  h :: FilePath -> IO FilePath
  h path = do
    is_dir <- doesDirectoryExist path
    return $ if is_dir
      then path </> "index.html"
      else path

picture_from_path :: FilePath -> Maybe String
picture_from_path path = do
  guard $ takeExtension path `elem` [".jpg", ".png", ".webm"]
  return $ takeBaseName path

parse_picture :: (MonadFail m) => FilePath -> m String
parse_picture path = fail_maybe ("invalid picture filename: " ++ path) $ picture_from_path path

slides_from_path :: FilePath -> Maybe String
slides_from_path path = do
  guard $ takeExtension path `elem` [".pdf"]
  return $ takeBaseName path

parse_slides :: (MonadFail m) => FilePath -> m String
parse_slides path = fail_maybe ("invalid slides path: " ++ path) $ slides_from_path path

-- Names.

data Name = Name
  { name_first :: String
  , name_last :: String
  } deriving (Eq, Show)

name_sort_key :: Name -> (String, (Maybe String, String))
name_sort_key name = (last_core, (tussenvoegsels, name_first name)) where
  (tussenvoegsels, last_core) = split_tussenvoegsels $ name_last name

name_sort_key_string :: Name -> String
name_sort_key_string first_last = unicode_sort_key_ascii s where
  s :: String
  s = intercalate ", " [last_, fromMaybe "_" tussenvoegsels, first]

  last_ :: String
  tussenvoegsels :: Maybe String
  first :: String
  (last_, (tussenvoegsels, first)) = name_sort_key first_last

instance Ord Name where
  compare = liftCompare2 compare_unicode (liftCompare2 (liftCompare compare_unicode) compare_unicode) `on` name_sort_key

instance FromJSON Name where
  parseJSON = withObject "Name" $ \v -> Name
    <$> v .: "first"
    <*> v .: "last"

format_name :: Name -> String
format_name = sequence [name_first, name_last] >>> intercalate " "

format_name_last_first :: Name -> String
format_name_last_first = sequence [name_last, name_first] >>> intercalate ", "

-- Persons.

data Person = Person
  { person_name :: Name
  , person_affiliation :: Maybe String
  , person_affiliation_short :: Maybe String
  , person_email :: Maybe String
  , person_homepage :: Maybe String
  , person_role :: Maybe String
  } deriving (Eq, Ord, Show)

instance FromJSON Person where
  parseJSON = withObject "Person" $ \v -> Person
    <$> parseJSON (Object  v)
    <*> v .:? "affiliation"
    <*> v .:? "affiliation_short"
    <*> v .:? "email"
    <*> v .:? "homepage"
    <*> v .:? "role"

person_affiliation_short_fallback :: Person -> Maybe String
person_affiliation_short_fallback person = person_affiliation_short person <|> person_affiliation person

person_options_affiliation_shorten :: Bool -> Person -> Maybe String
person_options_affiliation_shorten = \case
  False -> person_affiliation
  True -> person_affiliation_short_fallback

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

-- Reading JSON committee files

parse_file_committee :: FilePath -> IO [Person]
parse_file_committee = ByteString.readFile >=> decode_json >>> fmap sort

-- Reading the JSON invited talks file.

data Invited = Invited
  { invited_person :: Person
  , invited_title :: Maybe String
  , invited_title_latex :: Maybe String
  , invited_abstract :: Maybe String
  , invited_abstract_html :: Maybe String
  , invited_abstract_latex :: Maybe String
  , invited_chair :: Maybe String
  , invited_picture :: Maybe String
  , invited_slides :: Maybe String
  } deriving (Eq, Ord, Show)

instance FromJSON Invited where
  parseJSON = withObject "Invited" $ \v -> Invited
    <$> parseJSON (Object v)
    <*> v .:? "title"
    <*> v .:? "title_latex"
    <*> v .:? "abstract"
    <*> v .:? "abstract_html"
    <*> v .:? "abstract_latex"
    <*> v .:? "chair"
    <*> return Nothing
    <*> return Nothing

type Inviteds = Map String Invited

parse_file_inviteds :: FilePath -> IO Inviteds
parse_file_inviteds = ByteString.readFile >=> decode_json

type InvitedFiles = Map String FilePath

inviteds_with_pictures :: InvitedFiles -> Inviteds -> Inviteds
inviteds_with_pictures pictures = Map.mapWithKey $
    \id_ invited -> invited { invited_picture = Map.lookup id_ pictures }

inviteds_with_slides :: InvitedFiles -> Inviteds -> Inviteds
inviteds_with_slides slides = Map.mapWithKey $
    \id_ invited -> invited { invited_slides = Map.lookup id_ slides }

invited_speaker :: Invited -> String
invited_speaker = invited_person >>> person_name >>> format_name

-- Reading the JSON sessions file.

data Session = Session
  { session_id :: Integer
  , session_title :: String
  , session_title_short :: Maybe String
  , session_papers :: [Integer]
  , session_chair :: Maybe String
  } deriving (Eq, Show)

instance FromJSON Session where
  parseJSON = withObject "Session" $ \v -> Session
    <$> v .: "id"
    <*> v .: "title"
    <*> v .:? "title_short"
    <*> v .: "papers"
    <*> v .:? "chair"

session_title_short_maybe :: Session -> String
session_title_short_maybe = with_fallback session_title session_title_short

show_id :: Integer -> String
show_id = (+1) >>> show

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

title_short_maybe :: Title -> String
title_short_maybe = with_fallback title_string title_short

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

invited_key_by_schedule :: Schedule -> [String]
invited_key_by_schedule (Schedule schedule) = do
  day_schedule <- Map.elems schedule
  (_, EventInvitedTalk key) <- day_schedule
  return key

-- Reading the JSON papers file.

data Paper = Paper
  { paper_id :: Integer
  , paper_title :: String
  , paper_title_latex :: Maybe String
  , paper_authors :: [Person]
  , paper_path :: Maybe FilePath
  , paper_slides_path :: Maybe FilePath
  } deriving (Eq, Show)

instance Ord Paper where
  compare = liftCompare2 compare compare_unicode `on` (paper_authors &&& paper_title)

instance FromJSON Paper where
  parseJSON = withObject "Paper" $ \v -> do
    _type :: Text <- v .: "object"
    unless (_type == "paper") $ fail "not a paper"
    Paper
      <$> v .: "pid"
      <*> v .: "title"
      <*> v .:? "title_latex"
      <*> v .: "authors"
      <*> return Nothing
      <*> return Nothing

format_authors :: Paper -> String
format_authors = paper_authors >>> map (person_name >>> format_name) >>> intercalate ", "

type Papers = Map Integer Paper

parse_file_papers :: FilePath -> IO Papers
parse_file_papers = ByteString.readFile
  >=> decode_json
  >>> fmap (map (\s -> (paper_id s, s)))
  >=> to_map "papers" "id"

type PaperAbstracts = Map Integer FilePath

abstract_id_from_path :: FilePath -> Maybe Integer
abstract_id_from_path path = pdf <|> html_dir where
    pdf :: Maybe Integer
    pdf = do
      [file] <- return $ splitDirectories path
      let s = takeBaseName file
      let e = takeExtension file
      guard $ e == ".pdf"
      parse_integer s

    html_dir :: Maybe Integer
    html_dir = do
      [dir, "index.html"] <- return $ splitDirectories path
      parse_integer dir

parse_abstract :: (MonadFail m) => FilePath -> m Integer
parse_abstract path = fail_maybe ("invalid abstract filename: " ++ path) $ abstract_id_from_path path

parse_abstracts :: String -> FilePath -> IO PaperAbstracts
parse_abstracts kind dir = do
  u <- list_files_or_index_html dir
  map_from_list_unique_m ("could not parse " ++ kind) $ h u
  where
    h :: [FilePath] -> [(Integer, FilePath)]
    h paths = do
      path <- paths
      case abstract_id_from_path $ fromJust $ path_strip_prefix dir path of
        Nothing -> mempty
        Just id_ -> return (id_, path)

papers_with_abstract :: PaperAbstracts -> Papers -> Papers
papers_with_abstract abstracts = Map.mapWithKey $
    \id_ paper -> paper { paper_path = Map.lookup id_ abstracts}

papers_with_slides :: PaperAbstracts -> Papers -> Papers
papers_with_slides slides = Map.mapWithKey $
    \id_ paper -> paper { paper_slides_path = Map.lookup id_ slides}

papers_with_abstract_and_slides :: PaperAbstracts -> PaperAbstracts -> Papers -> Papers
papers_with_abstract_and_slides abstracts slides = papers_with_abstract abstracts >>> papers_with_slides slides

parse_papers :: FilePath -> Maybe FilePath -> Maybe FilePath -> IO Papers
parse_papers path_json path_abstracts path_slides = do
  papers <- parse_file_papers path_json
  papers1 <- case path_abstracts of
    Nothing -> return papers
    Just path -> do
      abstracts <- parse_abstracts "abstracts" path
      return $ papers_with_abstract abstracts papers
  papers2 <- case path_slides of
    Nothing -> return papers1
    Just path -> do
      slides <- parse_abstracts "slides" path
      return $ papers_with_slides slides papers1
  return papers2

-- Data.

talk_length :: NominalDiffTime
talk_length = time_of_day_to_diff $ Time.midnight {Time.todMin = 20}

schedule_unit :: NominalDiffTime
schedule_unit = time_of_day_to_diff $ TimeOfDay 1 0 0
