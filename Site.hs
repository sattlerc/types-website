{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow ((>>>))
import Control.Monad (forM)
import Data.Function ((&))
import Data.List (stripPrefix)
import Data.Monoid (mappend)
import Data.String (fromString)
import System.FilePath (makeRelative, replaceExtension, (</>))

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

papers_id :: Identifier
papers_id = "papers.json"

papers_compiler :: Compiler Papers
papers_compiler = data_compiler papers_id parse_file_papers

sessions_id :: Identifier
sessions_id = "sessions.json"

sessions_compiler :: Compiler Sessions
sessions_compiler = data_compiler sessions_id parse_file_sessions

schedule_id :: Identifier
schedule_id = "schedule.json"

schedule_compiler :: Compiler Schedule
schedule_compiler = data_compiler schedule_id parse_file_schedule

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

process_page :: Identifier -> Item String -> Compiler (Item String)
process_page identifier page = applyAsTemplate data_context page

page_compiler :: Compiler (Item String)
page_compiler = do
  page <- pandocCompiler
  identifier <- getUnderlying
  page <- process_page identifier page
  loadAndApplyTemplate "templates/default.html" navigation_context page

main :: IO ()
main = hakyll $ do
  -- Prevent page from being crawled.
  -- match "robots.txt" $ do
  --   route $ customRoute toFilePath
  --   compile copyFileCompiler

  -- README for website editors.
  -- Delete from generation once everyone has access to the sources.
  match "README.md" $ do
    route $ customRoute $ toFilePath >>> (`replaceExtension` "html")
    compile pandocCompiler

  -- Files that should just be copied over.
  -- Files in `monitor` are for monitoring website availability.
  match ("files/**" .||. "images/**" .||. "monitor/**") $ do
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

  -- This approach doesn't work because [Paper] is not writable.
  -- match (fromList [accepted_papers_id]) $ compile $ do
  --   bs <- getResourceLBS
  --   withItemBody (decode_json :: ByteString -> Compiler [Paper]) (bs :: Item ByteString)

  -- Pages of the conference website.
  match (("*.md" .||. "*.html") .&&. complement "README.md") $ do
    route $ customRoute $ toFilePath >>> (`replaceExtension` "html")
    compile page_compiler
