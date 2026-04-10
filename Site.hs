{-# LANGUAGE BlockArguments, ImportQualifiedPost, OverloadedStrings #-}
import Control.Arrow ((>>>))
import Control.Monad (forM, (>=>))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (stripPrefix)
import Data.Map qualified as Map
import Data.Monoid (mappend)
import Data.String (fromString)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (makeRelative, replaceExtension, takeBaseName, (</>))

import Hakyll

import Papers

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
        let id = itemBody item
        metadata <- getMetadata id
        let Just title = lookupString "title" metadata
        return title
    , field "url" $ \item -> do
        let id = itemBody item
        route <- getRoute id
        path <- case route of
          Just path -> return path
          Nothing -> fail $ "No route found for identifier " ++ show id
        return path
    ]

data_compiler :: Identifier -> (FilePath -> IO a) -> Compiler a
data_compiler id parse = do
  CopyFile path <- loadBody id
  unsafeCompiler $ parse path

path_abstracts :: FilePath
path_abstracts = "abstracts"

pattern_abstracts :: Pattern
pattern_abstracts = fromString $ path_abstracts ++ "/" ++ "*"

papers_id :: Identifier
papers_id = "data/papers.json"

papers_compiler :: Compiler Papers
papers_compiler = do
  abstracts <- getMatches pattern_abstracts
    >>= (traverse (toFilePath >>> parse_abstract))
    <&> Map.fromList
  papers <- data_compiler papers_id parse_file_papers
  return $ papers_with_abstract abstracts papers

sessions_id :: Identifier
sessions_id = "data/sessions.json"

sessions_compiler :: Compiler Sessions
sessions_compiler = data_compiler sessions_id parse_file_sessions

schedule_id :: Identifier
schedule_id = "data/schedule.json"

schedule_compiler :: Compiler Schedule
schedule_compiler = data_compiler schedule_id parse_file_schedule

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
  [ field "papers_list" $ const $ do
      papers <- papers_compiler
      return $ format_papers papers
  , field "programme_list" $ const $ do
      papers <- papers_compiler
      sessions <- sessions_compiler
      schedule <- schedule_compiler
      return $ format_schedule papers sessions schedule
  ]

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
  match ("files/**" .||. "images/**" .||. pattern_abstracts .||. "monitor/**") $ do
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
  match (fromList [papers_id, sessions_id, schedule_id]) $
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

