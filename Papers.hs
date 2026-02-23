{-# LANGUAGE DeriveAnyClass, DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}
module Papers where

import Prelude hiding (last)
import Control.Monad (forM_, unless, void)
import Control.Arrow ((>>>), (&&&))
import Data.Aeson (FromJSON, eitherDecode, eitherDecodeFileStrict, fromJSON, parseJSON, withObject, (.:))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import Data.Char (toLower)
import Data.Function (on)
import Data.List (intercalate, sort)
import Data.Maybe (fromJust)
import Data.Text.Lazy (Text, unpack)
import GHC.Generics (Generic)

import Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as Blaze
import qualified Text.Blaze.Html5.Attributes as Attributes
import qualified Text.Blaze.Html.Renderer.Pretty as BlazePretty
import qualified Text.Blaze.Html.Renderer.String as BlazeString

-- Reading the JSON file of papers.
-- Includes basic string formatting.

decode_json :: (FromJSON a, MonadFail m) => ByteString -> m a
decode_json bs = do
  case eitherDecode bs of
    Left msg -> fail msg
    Right x -> return x

data Author = Author
  { first :: String
  , last :: String
  , affiliation :: String
  } deriving (Eq, FromJSON, Generic, Show)

instance Ord Author where
  compare = compare `on` (lowercase . last) &&& (lowercase . first)
    where lowercase = map toLower

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
format_author author = intercalate " " [first author, last author]

format_authors :: Paper -> String
format_authors = authors >>> map format_author >>>  intercalate ", "

parse_papers :: FilePath -> IO [Paper]
parse_papers path = do
  bs <- ByteString.readFile path
  papers <- decode_json bs
  return $ sort papers


-- HTML rendering.

format_paper_html_li :: Paper -> String
format_paper_html_li paper = BlazeString.renderHtml $ do
  Blaze.li $ do
    Blaze.string $ format_authors paper
    Blaze.string ": "
    Blaze.i $ Blaze.string $ title paper

format_papers_html_ul :: [Paper] -> String
format_papers_html_ul = traverse (Blaze.preEscapedString . format_paper_html_li) >>> void >>> Blaze.ul >>> BlazePretty.renderHtml
