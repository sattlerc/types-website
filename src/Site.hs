{-# LANGUAGE BlockArguments, ImportQualifiedPost, OverloadedStrings #-}
import Control.Arrow ((>>>), (&&&))
import Control.Monad (forM, (>=>))
import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Maybe (fromJust)
import Data.String (fromString)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (replaceExtension, takeBaseName, (</>))

import Hakyll

import General
import Parse
import Paths qualified
import Render

pattern_disjunction :: [Pattern] -> Pattern
pattern_disjunction = foldr (.||.) (fromList [])

pattern_pdf :: FilePath -> Pattern
pattern_pdf dir = fromString $ dir </> "*.pdf"

pattern_pdf_or_html_dir :: FilePath -> Pattern
pattern_pdf_or_html_dir dir = (fromString $ dir </> "*.pdf") .||. (fromString $ dir </> "*/**")

pattern_pdf_or_html_dir_index :: FilePath -> Pattern
pattern_pdf_or_html_dir_index dir = (fromString $ dir </> "*.pdf") .||. (fromString $ dir </> "*/index.html")

pattern_static :: Pattern
pattern_static = pattern_disjunction
  [ "css/**"
  , "files/**"
  , "images/**"
  , pattern_pdf Paths.abstracts
  , pattern_pdf_or_html_dir Paths.slides
  , pattern_pdf_or_html_dir Paths.slides_invited
  ]

navigation_id :: Identifier
navigation_id = "templates/navigation.html"

navigation_context :: Context String
navigation_context = defaultContext <> field "navigation" (const $ loadBody navigation_id)

navigation_compiler :: Compiler (Item String)
navigation_compiler = do
  metadata <- getUnderlying >>= getMetadata
  body <- getResourceBody
  Just navigation_ids <- return $ lookupStringList "navigation_ids" metadata
  let context = listField "navigation_items" nav_item_context $ forM navigation_ids $ fromFilePath >>> makeItem
  applyAsTemplate context body
  where
  nav_item_context :: Context Identifier
  nav_item_context = mconcat
    [ field "title" $ \item -> do
        let id_ = itemBody item
        metadata <- getMetadata id_
        Just title <- return $ lookupString "title" metadata
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

papers_compiler :: Compiler Papers
papers_compiler = papers_with_abstract_and_slides
  <$> parse_directory "abstracts" parse_abstract Paths.abstracts (pattern_pdf Paths.abstracts)
  <*> parse_directory "slides" parse_abstract Paths.slides (pattern_pdf_or_html_dir_index Paths.slides)
  <*> data_compiler parse_file_papers (fromString Paths.papers)

inviteds_compiler :: Compiler Inviteds
inviteds_compiler = do
  inviteds <- data_compiler parse_file_inviteds $ fromString Paths.inviteds
  pictures <- parse_directory "pictures" parse_picture "images/invited" "images/invited/*"
  slides <- parse_directory "invited slides" parse_slides Paths.slides_invited $ pattern_pdf_or_html_dir_index Paths.slides_invited
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

type Includes = [(String, Identifier)]

list_includes :: IO Includes
list_includes = do
  include_exists <- doesDirectoryExist Paths.include
  if not include_exists
    then return []
    else listDirectory Paths.include <&> map \file ->
      ( "include_" ++ takeBaseName file
      , fromFilePath $ Paths.include </> file
      )

include_context :: Includes -> Context String
include_context = map include_field >>> mconcat
  where
  include_field (key, identifier) = field key $ const $ loadBody identifier

data_context :: Context String
data_context = mconcat $ map to_context fields where
  options_local :: PersonOptions
  options_local = options_base

  options_other :: PersonOptions
  options_other = options_base { person_options_homepage = False }

  fields :: [(String, Compiler String)]
  fields =
    [ ("papers_list", format_papers <$> papers_compiler)
    , ("invited_list", format_invited_speakers <$> inviteds_compiler)
    , ("programme_list",format_schedule <$> papers_compiler <*> inviteds_compiler <*> sessions_compiler <*> schedule_compiler)
    , ("programme_table",format_schedule_table <$> inviteds_compiler <*> sessions_compiler <*> schedule_compiler)
    , ("organizing_committee",format_committee options_local <$> organizing_committee_compiler)
    , ("program_committee", format_committee options_other <$> program_committee_compiler)
    , ("steering_committee", format_committee options_other <$> steering_committee_compiler)
    ]

  to_context :: (String, Compiler String) -> Context String
  to_context (name, compiler) = field name $ const compiler

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
  match pattern_static $ do
    route $ customRoute $ toFilePath
    compile copyFileCompiler

  -- Templates.
  match "templates/default.html" $ compile templateBodyCompiler

  -- Navigation bar.
  -- Usable in templates via navigation_context.
  match (fromList [navigation_id]) $ compile navigation_compiler

  -- Data.
  match "data/*.json" $ compile copyFileCompiler

  -- Includes.
  match "include/**" $ compile page_compiler

  -- This approach doesn't work because [Paper] is not writable.
  -- match (fromList [accepted_papers_id]) $ compile $ do
  --   bs <- getResourceLBS
  --   withItemBody (decode_json :: ByteString -> Compiler [Paper]) (bs :: Item ByteString)

  -- Pages of the conference website.
  match ("pages/**.md" .||. "pages/**.html") $ do
    route $ customRoute $ toFilePath >>> path_strip_prefix "pages" >>> fromJust >>> (`replaceExtension` "html")
    compile $ page_compiler >>= loadAndApplyTemplate "templates/default.html" navigation_context
