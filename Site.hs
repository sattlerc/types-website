{-# LANGUAGE BlockArguments, ImportQualifiedPost, OverloadedStrings #-}
import Control.Arrow ((>>>), (&&&))
import Control.Monad (forM, (>=>))
import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.String (fromString)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (replaceExtension, takeBaseName, (</>))

import Hakyll

import General
import Parse
import Paths qualified
import Render

pairA :: (Applicative f) => (f a, f b) -> f (a, b)
pairA = uncurry $ liftA2 (,)

navigation_id :: Identifier
navigation_id = "templates/navigation.html"

navigation_context :: Context String
navigation_context = defaultContext <> field "navigation" (const $ loadBody navigation_id)

navigation_compiler :: Compiler (Item String)
navigation_compiler = do
  metadata <- getUnderlying >>= getMetadata
  body <- getResourceBody
  let Just navigation_ids = lookupStringList "navigation_ids" metadata
  let context = listField "navigation_items" nav_item_context $ forM navigation_ids $ fromFilePath >>> makeItem
  applyAsTemplate context body
  where
  nav_item_context :: Context Identifier
  nav_item_context = mconcat
    [ field "title" $ \item -> do
        let id_ = itemBody item
        metadata <- getMetadata id_
        let Just title = lookupString "title" metadata
        return title
    , field "url" $ \item -> do
        let id_ = itemBody item
        route_ <- getRoute id_
        path <- case route_ of
          Just path -> return path
          Nothing -> fail $ "No route found for identifier " ++ show id_
        return path
    ]

data_compiler :: (FilePath -> IO a) -> Identifier -> Compiler a
data_compiler parse id_ = do
  CopyFile path <- loadBody id_
  unsafeCompiler $ parse path

parse_directory :: (Ord a, Show a) => String -> (FilePath -> Compiler a) -> FilePath -> Pattern -> Compiler (Map a FilePath)
parse_directory kind parse_file base_dir = (getMatches :: Pattern -> Compiler [Identifier])
  >=> traverse (toFilePath >>> (path_strip_prefix base_dir >>> fromJust >>> parse_file) &&& return >>> pairA)
  >=> map_from_list_unique_m ("could not parse " ++ kind)

pattern_pdf :: FilePath -> Pattern
pattern_pdf dir = fromString $ dir </> "*.pdf"

pattern_pdf_or_html_dir :: FilePath -> Pattern
pattern_pdf_or_html_dir dir = (fromString $ dir </> "*.pdf") .||. (fromString $ dir </> "*/**")

pattern_pdf_or_html_dir_index :: FilePath -> Pattern
pattern_pdf_or_html_dir_index dir = (fromString $ dir </> "*.pdf") .||. (fromString $ dir </> "*/index.html")

pattern_abstracts :: Pattern
pattern_abstracts = pattern_pdf Paths.abstracts

pattern_slides :: Pattern
pattern_slides = pattern_pdf_or_html_dir Paths.slides

pattern_slides_index :: Pattern
pattern_slides_index = pattern_pdf_or_html_dir_index Paths.slides

pattern_slides_invited :: Pattern
pattern_slides_invited = pattern_pdf_or_html_dir Paths.slides_invited

pattern_slides_invited_index :: Pattern
pattern_slides_invited_index = pattern_pdf_or_html_dir_index Paths.slides_invited

papers_compiler :: Compiler Papers
papers_compiler = papers_with_abstract_and_slides
  <$> parse_directory "abstracts" parse_abstract Paths.abstracts pattern_abstracts
  <*> parse_directory "slides" parse_abstract Paths.slides pattern_slides_index
  <*> data_compiler parse_file_papers (fromString Paths.papers)

inviteds_compiler :: Compiler Inviteds
inviteds_compiler = do
  inviteds <- data_compiler parse_file_inviteds $ fromString Paths.inviteds
  pictures <- parse_directory "pictures" parse_picture "images/invited" "images/invited/*"
  slides <- parse_directory "invited slides" parse_slides Paths.slides_invited pattern_slides_invited_index
  return $ inviteds_with_slides slides $ inviteds_with_pictures pictures inviteds

sessions_compiler :: Compiler Sessions
sessions_compiler = data_compiler parse_file_sessions $ fromString Paths.sessions

schedule_compiler :: Compiler Schedule
schedule_compiler = data_compiler parse_file_schedule $ fromString Paths.schedule

organizing_committee_compiler :: Compiler [Person]
organizing_committee_compiler = data_compiler parse_file_committee $ fromString Paths.organizing_committee

program_committee_compiler :: Compiler [Person]
program_committee_compiler = data_compiler parse_file_committee $ fromString Paths.program_committee

steering_committee_compiler :: Compiler [Person]
steering_committee_compiler = data_compiler parse_file_committee $ fromString Paths.steering_committee

dir_include :: FilePath
dir_include = "include"

type Includes = [(String, Identifier)]

list_includes :: IO Includes
list_includes = do
  include_exists <- doesDirectoryExist dir_include
  if not include_exists
    then return []
    else listDirectory dir_include <&> map \file ->
      ( "include_" ++ takeBaseName file
      , fromFilePath $ dir_include </> file
      )

include_context :: Includes -> Context String
include_context = map include_field >>> mconcat
  where
  include_field (key, identifier) = field key $ const $ loadBody identifier

data_context :: Context String
data_context = mconcat
  [ field "papers_list" $ const $
      format_papers <$> papers_compiler
  , field "invited_list" $ const $
      format_invited_speakers <$> inviteds_compiler
  , field "programme_list" $ const $
      format_schedule <$> papers_compiler <*> inviteds_compiler <*> sessions_compiler <*> schedule_compiler
  , field "programme_table" $ const $
      format_schedule_table <$> inviteds_compiler <*> sessions_compiler <*> schedule_compiler
  , field "organizing_committee" $ const $
      format_committee options_local <$> organizing_committee_compiler
  , field "program_committee" $ const $
      format_committee options_other <$> program_committee_compiler
  , field "steering_committee" $ const $
      format_committee options_other <$> steering_committee_compiler
  ]
  where
  options_local :: PersonOptions
  options_local = options_base

options_other :: PersonOptions
options_other = options_base { person_options_homepage = False }

page_compiler :: Compiler (Item String)
page_compiler = do
  page <- pandocCompiler
  includes <- unsafeCompiler list_includes
  let context = include_context includes <> data_context
  applyAsTemplate context page

main :: IO ()
main = hakyll $ do
  -- Prevent page from being crawled.
  -- match "robots.txt" $ do
  --   route $ customRoute toFilePath
  --   compile copyFileCompiler

  -- README for website editors.
  -- Consider deleting from generation once everyone has access to the sources.
  match "README.md" $ do
    route $ customRoute $ toFilePath >>> (`replaceExtension` "html")
    compile pandocCompiler

  -- Files that should just be copied over.
  -- Files in `monitor` are for monitoring website availability.
  match ("css/**" .||. "images/**" .||. pattern_abstracts .||. pattern_slides .||. pattern_slides_invited .||. "files/**" .||. "monitor/**") $ do
    route $ customRoute $ toFilePath
    compile copyFileCompiler

  -- Templates.
  match "templates/default.html" $
    compile templateBodyCompiler

  -- Navigation bar.
  -- Usable in templates via navigation_context.
  match (fromList [navigation_id]) $
    compile navigation_compiler

  -- Data.
  match (fromList $ map fromString [Paths.papers, Paths.inviteds, Paths.sessions, Paths.schedule, Paths.organizing_committee, Paths.program_committee, Paths.steering_committee]) $
    compile copyFileCompiler

  -- Includes.
  match "include/**" $
    compile page_compiler  

  -- This approach doesn't work because [Paper] is not writable.
  -- match (fromList [accepted_papers_id]) $ compile $ do
  --   bs <- getResourceLBS
  --   withItemBody (decode_json :: ByteString -> Compiler [Paper]) (bs :: Item ByteString)

  -- Pages of the conference website.
  match (("*.md" .||. "*.html") .&&. complement ("README.md" .||. "INSTALL.md")) $ do
    route $ customRoute $ toFilePath >>> (`replaceExtension` "html")
    compile $ page_compiler >>= loadAndApplyTemplate "templates/default.html" navigation_context

