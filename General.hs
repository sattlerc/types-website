module General where

import Control.Arrow ((>>>), (&&&), (***))
import Data.Char (isLower, isSpace)
import Data.Function (on)
import Data.List (dropWhileEnd, groupBy)
import Data.Maybe (fromMaybe)
import Data.Time (Day, NominalDiffTime, TimeOfDay)
import Data.Time qualified as Time
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.Format.ISO8601 qualified as ISO8601

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
