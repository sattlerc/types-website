{-# LANGUAGE DeriveAnyClass, DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}
module Papers where

import Prelude hiding (last)
import Control.Monad ((>=>), forM_, unless, void)
import Control.Arrow ((>>>))
import Data.Aeson (FromJSON, eitherDecode, eitherDecodeFileStrict, fromJSON, parseJSON, withObject, (.:))
import Data.Aeson.Types (Parser, Value)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import Data.Char (toLower)
import Data.Function ((&), on)
import Data.List (intercalate, sort)
import Data.Maybe (fromJust)
import Data.Map (Map)
import qualified Data.Map
import Data.Text.Lazy (Text, unpack)
import Data.Time (Day, TimeOfDay)
import qualified Data.Time.Format.ISO8601 as ISO8601
import GHC.Generics (Generic)

import Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as Blaze
import qualified Text.Blaze.Html5.Attributes as Attributes
import qualified Text.Blaze.Html.Renderer.Pretty as BlazePretty
import qualified Text.Blaze.Html.Renderer.String as BlazeString

-- Data.Time.Format.ISO8601

-- Reading the JSON file of sessions

data Session = Session
  { session_id :: Integer
  , session_title :: String
  , session_papers :: [Integer]
  } deriving (Eq, FromJSON, Generic, Show)

instance Ord Session where
  compare = compare `on` id

parse_sessions :: FilePath -> IO (Map Integer Session)
parse_sessions = ByteString.readFile >=> decode_json >>> fmap (map (\s -> (session_id s, s)) >>> Data.Map.fromList)


type TimeOfDayRange = (TimeOfDay, TimeOfDay)

--  { range :: TimeOfDayRange
--  , 

data Event = EventSpecial { event_special_title :: Text }
           | EventBreak { event_break_title :: Text }
           | EventSession { event_session_id :: Integer }
  deriving (Eq, Show)

newtype RangedEvent = RangedEvent { ranged_event :: (TimeOfDayRange, Event) }
  deriving (Eq, Show)

time_of_day_format = ISO8601.hourMinuteFormat ISO8601.ExtendedFormat

time_of_day_parse :: (MonadFail m) => String -> m TimeOfDay
time_of_day_parse = ISO8601.formatParseM time_of_day_format

instance Ord RangedEvent where
  compare = compare `on` ranged_event >>> fst

parse_ranged_event :: Value -> Parser (TimeOfDayRange, Event)
parse_ranged_event = withObject "RangedEvent" $ \v -> do
  _from <- v .: "from" >>= time_of_day_parse
  _to <- v .: "to" >>= time_of_day_parse
  _type :: Text <- v .: "type"
  event <- case _type of
    "special" -> v .: "title" & fmap EventSpecial
    "break" -> v .: "title" & fmap EventBreak
    "session" -> v .: "id" & fmap EventSession
    _ -> fail $ sc"unknown event type " ++ unpack _type
  return $ RangedEvent ((_from, _to), event)

day_format = ISO8601.hourMinuteFormat ISO8601.ExtendedFormat

day_parse :: (MonadFail m) => String -> m TimeOfDay
day_parse = ISO8601.iso8601ParseM time_of_day_format

type Schedule = Map Day [(TimeOfDayRange, Event)]

parse_schedule :: Value -> Parser Schedule
parse_schedule = withObject "Schedule" $ do
  days <- parse_days
  
  where
  parse_day = do
    _day <- v .: "day" >>= ISO8601.iso8601ParseM
    ranged_events <- v .: "events"
    return (_day, ranged_events)

-- Reading the JSON file of papers.
-- Includes basic string formatting.

decode_json :: (FromJSON a, MonadFail m) => ByteString -> m a
decode_json bs = case eitherDecode bs of
  Left msg -> fail msg
  Right x -> return x

data Author = Author
  { first :: String
  , last :: String
  , affiliation :: String
  } deriving (Eq, FromJSON, Generic, Show)

lowercase :: String -> String
lowercase = map toLower

instance Ord Author where
  compare = compare `on` sequence [last, first] >>> map lowercase

data Paper = Paper
  { identifier :: Integer
  , title :: String
  , authors :: [Author]
  } deriving (Eq, Show)

instance Ord Paper where
  compare = compare `on` authors

instance FromJSON Paper where
  parseJSON = withObject "Paper" $ \v -> do
    _type :: Text <- v .: "object"
    unless (_type == "paper") $ fail "not a paper"
    _identifier <- v .: "pid"
    _title <- v .: "title"
    _authors <- v .: "authors"
    return $ Paper
      { identifier = _identifier
      , title = _title
      , authors = _authors
      }

format_author :: Author -> String
format_author = sequence [first, last] >>> intercalate " "

format_authors :: Paper -> String
format_authors = authors >>> map format_author >>>  intercalate ", "

parse_papers :: FilePath -> IO [Paper]
parse_papers = ByteString.readFile >=> decode_json >>> fmap sort


-- HTML rendering.

format_paper_html_li :: Paper -> String
format_paper_html_li paper = BlazeString.renderHtml $ do
  Blaze.li $ do
    Blaze.string $ format_authors paper
    Blaze.string ": "
    Blaze.i $ Blaze.string $ title paper

format_papers_html_ul :: [Paper] -> String
format_papers_html_ul = traverse (Blaze.preEscapedString . format_paper_html_li) >>> void >>> Blaze.ul >>> BlazePretty.renderHtml
