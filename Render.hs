{-# LANGUAGE ImportQualifiedPost, LambdaCase, OverloadedStrings #-}
module Render where

import Control.Arrow ((>>>))
import Control.Monad (void, when)
import Control.Monad.State (State, get, runState, put)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.List (intercalate, sort)
import Data.Map qualified as Map
import Data.Maybe (fromJust, isJust)
import Data.String (fromString)
import Data.Time (Day, TimeOfDay)
import Data.Time qualified as Time

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
format_schedule papers inviteds sessions = unschedule
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
      EventSession id_ -> format_session (sessions Map.! id_)
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
      let paper_end = time_add talk_length start
      put paper_end
      return $ do
        Blaze.string $ format_time_range (paper_start, paper_end)
        Blaze.string ": "
        format_paper paper

    format_session :: Session -> Html
    format_session session =
      Blaze.li $ do
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
