{-# LANGUAGE BlockArguments, ImportQualifiedPost, OverloadedStrings #-}
import Control.Arrow ((>>>), (&&&))
import Control.Monad (forM, (>=>))
import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.String (fromString)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (replaceExtension, takeBaseName, (</>))

import Hakyll

import Parse
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

parse_directory :: (Ord a) => (FilePath -> Compiler a) -> Pattern -> Compiler (Map a FilePath)
parse_directory parse_file = (getMatches :: Pattern -> Compiler [Identifier])
  >=> traverse (toFilePath >>> parse_file &&& return >>> pairA)
  >>> fmap Map.fromList

path_abstracts :: FilePath
path_abstracts = "abstracts"

pattern_abstracts :: Pattern
pattern_abstracts = fromString $ path_abstracts ++ "/" ++ "*.pdf"

papers_id :: Identifier
papers_id = "data/papers.json"

papers_compiler :: Compiler Papers
papers_compiler = papers_with_abstract
  <$> parse_directory parse_abstract pattern_abstracts
  <*> data_compiler parse_file_papers papers_id

inviteds_id :: Identifier
inviteds_id = "data/invited.json"

inviteds_compiler :: Compiler Inviteds
inviteds_compiler = inviteds_with_pictures
  <$> parse_directory parse_picture "images/invited/*"
  <*> data_compiler parse_file_inviteds inviteds_id

sessions_id :: Identifier
sessions_id = "data/sessions.json"

sessions_compiler :: Compiler Sessions
sessions_compiler = data_compiler parse_file_sessions sessions_id

schedule_id :: Identifier
schedule_id = "data/schedule.json"

schedule_compiler :: Compiler Schedule
schedule_compiler = data_compiler parse_file_schedule schedule_id

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
  match ("css/**" .||. "images/**" .||. pattern_abstracts .||. "files/**" .||. "monitor/**") $ do
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
  match (fromList [papers_id, inviteds_id, sessions_id, schedule_id]) $
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

