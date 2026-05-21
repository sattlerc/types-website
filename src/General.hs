module General where

import Control.Arrow ((>>>), (&&&), (***), first, second)
import Control.Monad (guard)
import Data.ByteString.Builder qualified as ByteString
import Data.ByteString.Lazy.Char8 qualified as ByteString
import Data.Char (isLower, isNumber, isSpace)
import Data.Function (on)
import Data.List (dropWhileEnd, groupBy, stripPrefix, uncons)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (pack)
import Data.Time (Day, NominalDiffTime, TimeOfDay)
import Data.Time qualified as Time
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.Format.ISO8601 qualified as ISO8601
import Data.Word (Word16)
import System.Directory (listDirectory)
import System.FilePath ((</>), joinPath, splitDirectories)
import Text.Collate as Collate

-- Utilities

-- strip_suffix :: (Eq a) => [a] -> [a] -> Maybe [a]
-- strip_suffix suffix = reverse >>> stripPrefix (reverse suffix) >>> fmap reverse

with_fallback :: (a -> b) -> (a -> Maybe b) -> (a -> b)
with_fallback f g = (f &&& g) >>> uncurry fromMaybe

split_at :: (a -> Bool) -> [a] -> [[a]]
split_at p = groupBy ((==) `on` p) >>> filter (head >>> p >>> not)

trim :: String -> String
trim = dropWhileEnd isSpace >>> dropWhile isSpace

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

split_tussenvoegsels :: String -> (Maybe String, String)
split_tussenvoegsels = words >>> span (all isLower) >>> (f *** unwords) where
  f :: [String] -> Maybe String
  f [] = Nothing
  f xs = Just $ unwords xs

unicode_sort_key :: String -> Collate.SortKey
unicode_sort_key = pack >>> Collate.sortKey "en-US"

sort_key_ascii :: Collate.SortKey -> String
sort_key_ascii = (\(Collate.SortKey ws) -> ws)
  >>> map ByteString.word16BE
  >>> mconcat
  >>> ByteString.toLazyByteString
  >>> ByteString.lazyByteStringHex
  >>> ByteString.toLazyByteString
  >>> ByteString.unpack

unicode_sort_key_ascii :: String -> String
unicode_sort_key_ascii = unicode_sort_key >>> sort_key_ascii

compare_unicode :: String -> String -> Ordering
compare_unicode = Collate.collate "en-US" `on` pack

{- Using text-icu
unicode_sort_key_ascii :: String -> IO String
unicode_sort_key_ascii s = do
  collator <- Collate.open "en-US"
  key <- Collate.sortKey collator $ pack s
  return $ ByteString.unpack $ ByteString.toLazyByteString $ ByteString.byteStringHex key
-}

list_directory :: FilePath -> IO [FilePath]
list_directory path = do
  entries <- listDirectory path
  return $ map (path </>) entries

path_strip_prefix :: FilePath -> FilePath -> Maybe FilePath
path_strip_prefix = curry $ (splitDirectories *** splitDirectories >>> uncurry stripPrefix) >>> fmap joinPath

update_head :: (a -> a) -> [a] -> [a]
update_head f = uncons >>> fromJust >>> first f >>> uncurry (:)

parse_integer :: String -> Maybe Integer
parse_integer s = do
  guard $ all isNumber s
  return $ read s

multimap :: (Ord k) => [(k, a)] -> Map k [a]
multimap = map (second return) >>> Map.fromListWith (++)

map_from_multimap :: (Ord k) => Map k [a] -> Either (k, (a, a)) (Map k a)
map_from_multimap = Map.traverseWithKey $ \k xs -> case xs of
  x : y : _ -> Left (k, (x, y))
  [x] -> Right x

map_from_list_unique :: (Ord k) => [(k, a)] -> Either (k, (a, a)) (Map k a)
map_from_list_unique = multimap >>> map_from_multimap

map_from_list_unique_m :: (MonadFail m, Ord k, Show k, Show a) => String -> [(k, a)] -> m (Map k a)
map_from_list_unique_m msg xs = case map_from_list_unique xs of
  Left (k, (x, y)) -> fail $ msg ++ ": key " ++ show k ++ " has duplicate values " ++ show x ++ " and " ++ show y
  Right r -> return r

pairA :: (Applicative f) => (f a, f b) -> f (a, b)
pairA = uncurry $ liftA2 (,)
