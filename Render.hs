{-# LANGUAGE ImportQualifiedPost, LambdaCase, OverloadedStrings #-}
module Render where

import Control.Arrow ((>>>))
import Control.Monad (guard, void, when)
import Control.Monad.State (State, get, runState, put)
import Data.Foldable (toList)
import Data.Function ((&), on)
import Data.Functor ((<&>))
import Data.List (intercalate, sort)
import Data.Map qualified as Map
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.String (fromString)
import Data.Time (Day, TimeOfDay, NominalDiffTime, todHour)
import Data.Time qualified as Time
import Data.Time.Format.ISO8601 qualified as ISO8601

import Text.Blaze.Html.Renderer.Pretty qualified as BlazePretty
import Text.Blaze.Html.Renderer.String qualified as BlazeString
import Text.Blaze.Html5 (Html)
import Text.Blaze.Html5 qualified as Blaze
import Text.Blaze.Html5.Attributes qualified as BlazeAttr

import Parse

-- Needed for preserving whitespace.
blaze_strict :: Html -> Html
blaze_strict = BlazeString.renderHtml >>> Blaze.preEscapedString

blaze_li_strict :: Html -> Html
blaze_li_strict = Blaze.li >>> blaze_strict

blaze_ul :: [Html] -> Html
blaze_ul = map Blaze.li >>> mconcat >>> Blaze.ul

blaze_ul_strict :: [Html] -> Html
blaze_ul_strict = map (Blaze.li >>> blaze_strict) >>> mconcat >>> Blaze.ul

blaze_classes :: [String] -> Blaze.Attribute
blaze_classes = intercalate " " >>> fromString >>> BlazeAttr.class_

blaze_styles :: [String] -> Blaze.Attribute
blaze_styles = intercalate "; " >>> fromString >>> BlazeAttr.style

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

css_attr :: String -> String -> String
css_attr attr value = attr ++ ": " ++ value

css_attr_length :: String -> String -> Double -> String
css_attr_length attr unit value = css_attr attr $ show value ++ unit

-- Papers.

format_author :: Author -> String
format_author = sequence [author_first, author_last] >>> intercalate " "

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
  Blaze.! blaze_classes ["achnor", "row", "border", "rounded", "m-1"] $ do
    Blaze.div Blaze.! blaze_classes ["col-md-auto", "m-2"] $
      case invited_picture invited of
        Nothing -> Blaze.div Blaze.! blaze_styles ["width: 200px"] $ return ()
        Just picture -> Blaze.img
          Blaze.! BlazeAttr.src (fromString picture)
          Blaze.! BlazeAttr.alt (fromString $ invited_speaker invited)
          Blaze.! blaze_classes ["img-fluid"]
          Blaze.! BlazeAttr.width "200px"
    Blaze.div Blaze.! blaze_classes ["col", "m-2"] $ do
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
  black = (Blaze.! blaze_styles ["color: black"])

format_invited_speakers :: Inviteds -> String
format_invited_speakers = Map.toAscList
  >>> map (uncurry format_invited_speaker)
  >>> mconcat
  >>> Blaze.div Blaze.! blaze_classes ["vstack", "gap-3", "mb-4"]
  >>> BlazePretty.renderHtml

-- Schedule table

session_anchor :: Integer -> String
session_anchor id_ = "session-" ++ show id_

format_schedule_table :: Papers -> Inviteds -> Sessions -> Schedule -> String
format_schedule_table papers inviteds sessions (Schedule schedule) = BlazePretty.renderHtml format
  where
  times :: (TimeOfDayRange -> TimeOfDay) -> [TimeOfDay]
  times f = do
    day_schedule <- toList schedule
    (range, _) <- day_schedule
    return $ f range

  unify :: (TimeOfDayRange -> TimeOfDay) -> ([Rational] -> Rational) -> TimeOfDay
  unify f h = times f
    & map (time_of_day_to_diff >>> (`time_ratio` schedule_unit))
    & h
    & (`time_scale` schedule_unit)
    & diff_to_time_of_day

  day_start :: TimeOfDay
  day_start = unify fst $ minimum -- >>> floor >>> fromInteger

  day_end :: TimeOfDay
  day_end = unify snd $ maximum -- >>> ceiling >>> fromInteger

  pos :: TimeOfDay -> Rational
  pos = time_of_day_to_diff
    >>> (subtract $ time_of_day_to_diff day_start)
    >>> flip time_ratio schedule_unit

  height_unit :: String
  height_unit = "px"

  height_heading :: Rational
  height_heading = 30

  height_row :: Rational
  height_row = 60

  pos_unit :: TimeOfDay -> Rational
  pos_unit = pos >>> (height_row *) >>> (height_heading +)

  css_vert_origin :: String
  css_vert_origin = "top: 0px"

  css_attr_height :: String -> Rational -> Rational -> String
  css_attr_height unit margin value = css_attr "height" $ show (fromRational (value + margin) :: Double) ++ unit

  css_vert_pos :: String -> TimeOfDay -> String
  css_vert_pos attr = pos_unit >>> fromRational >>> css_attr_length attr height_unit

  css_height :: TimeOfDayRange -> String
  css_height = uncurry (on subtract pos_unit) >>> fromRational >>> css_attr_height height_unit 1

  header_style :: [String]
  header_style =
    [ css_vert_origin
    , css_vert_pos "height" day_start
    , css_attr "background-color" "plum"
    ]

  cell_style :: TimeOfDayRange -> [String]
  cell_style (start, end) =
    [ css_vert_pos "top" start
    , css_height (start, end)
    , css_attr "border-color" "gray"
    ]

  cell :: Maybe String -> [String] -> [String] -> Html -> Html
  cell link classes styles = maybe Blaze.div blaze_link link
    Blaze.! blaze_classes (["list", "position-absolute", "border-top", "border-bottom", "border-1", "rounded-0"] ++ classes)
    Blaze.! blaze_styles (["display: block", "text-decoration: inherit", "color: inherit", "width: 100%"] ++ styles)

  column :: [String] -> [String] -> Html -> Html
  column classes styles = Blaze.div
    Blaze.! blaze_classes (["list-group", "col-lg", "position-relative", "border-end", "border-1", "border-dark", "rounded-0"] ++ classes)
    Blaze.! blaze_styles s
    where
    s =
      [ "width: 100%"
      , css_vert_origin
      , css_vert_pos "height" day_end
      , css_attr "background-color" "lightgray"
      ] ++ styles

  event_color :: Event -> String
  event_color = \case
    EventBreak _ -> "lightyellow"
    EventInvitedTalk _ -> "lightcyan"
    EventSession _ -> "lightblue"
    EventSpecial _ -> "lightgreen"

  event_link :: Event -> Maybe String
  event_link = \case
    EventBreak _ -> Nothing
    EventInvitedTalk key -> Just $ invited_speaker_link key
    EventSession id_ -> Just $ anchorize "" $ session_anchor id_
    EventSpecial title -> title_link title

  format_title :: Title -> String
  format_title title = fromMaybe (title_string title) $ title_short title

  format_ranged_event :: (TimeOfDayRange, Event) -> Html
  format_ranged_event (range@(start, _), event) = cell (event_link event) classes styles $ do
    blaze_strict $ Blaze.div $ Blaze.string $ case event of
      EventBreak title -> format_title title
      EventInvitedTalk key -> invited_speaker (inviteds Map.! key)
      EventSession id_ -> "Session " ++ show_id id_
      EventSpecial title -> format_title title
    blaze_strict $ Blaze.div $ Blaze.string $ time_show start
    where
    duration :: NominalDiffTime
    duration = uncurry time_of_day_diff range

    tiny :: Bool
    tiny = duration <= 15 * 60

    small :: Bool
    small = duration <= 30 * 60

    classes :: [String]
    classes = ["d-flex", "justify-content-between"] ++ if small
      then ["px-2", "z-1"]
      else if small
      then ["px-2", "py-1"]
      else ["p-2"]

    styles :: [String]
    styles = cell_style range ++ [css_attr "background-color" $ event_color event] ++ ["font-size: x-small" | tiny]

  format_day :: Day -> DaySchedule -> Html
  format_day date ranged_events = column [] [] $ do
    cell (Just $ anchorize "" $ ISO8601.iso8601Show date)
      ["d-flex", "align-items-center", "justify-content-center", "border-dark"]
      header_style $
      Blaze.b $ Blaze.string $ show_day_detailed date
    mconcat $ map format_ranged_event ranged_events

  format :: Html
  format = Blaze.div
    Blaze.! BlazeAttr.id "prgramme-table"
    Blaze.! blaze_classes ["anchor", "d-flex", "border-start", "border-bottom", "border-1", "border-dark", "max-w-auto"]
    Blaze.! blaze_styles ["max-width: 800px"]
    $ mconcat $ map (uncurry format_day) $ Map.toAscList schedule

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
format_schedule papers inviteds sessions (Schedule schedule) = BlazePretty.renderHtml format
  where
  format_ranged_event :: (TimeOfDayRange, Event) -> Html
  format_ranged_event (range@(start, end), event) = do
    case event of
      EventBreak title -> inline $ format_title title
      EventInvitedTalk key -> inline $ format_invited_talk key $ inviteds Map.! key
      EventSession id_ -> format_session $ sessions Map.! id_
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
      paper_start <- get
      let paper_end = time_add talk_length paper_start
      put paper_end
      return $ do
        Blaze.string $ format_time_range (paper_start, paper_end)
        Blaze.string ": "
        format_paper paper

    format_session :: Session -> Html
    format_session session =
      Blaze.li
        Blaze.! BlazeAttr.id (fromString $ session_anchor id_)
        Blaze.! blaze_classes ["anchor"] $ do
        blaze_strict $ prefix <> (Blaze.string $ "Session " ++ show_id id_)
        blaze_ul_strict items_checked
      where
      id_ = session_id session
      (items, end_computed) = session & session_papers &
        traverse ((papers Map.!) >>> format_talk) &
        flip runState start
      items_checked = if end_computed == end
        then items
        else error $ "session " ++ show_id id_ ++ " has bad length: "
             ++ "ends at " ++ time_show end ++ ", "
             ++ "but talks end at " ++ time_show end_computed

  format_day :: Day -> DaySchedule -> Html
  format_day date ranged_events = do
    Blaze.h3
      Blaze.! BlazeAttr.id (fromString $ ISO8601.iso8601Show date)
      Blaze.! blaze_classes ["anchor"] $
      Blaze.string $ show_day_detailed date
    Blaze.ul $ mconcat $ map format_ranged_event ranged_events

  format :: Html
  format = Blaze.section
    Blaze.! BlazeAttr.id "programme_list"
    $ mconcat $ map (uncurry format_day) $ Map.toAscList schedule
