{-# LANGUAGE OverloadedStrings #-}
import Prelude
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
navigation_context = defaultContext <> field "navigation" (\_ -> loadBody navigation_id)

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

accepted_papers_id :: Identifier
accepted_papers_id = "types26-confdata.json"

accepted_papers :: Compiler [Paper]
accepted_papers = do
  CopyFile path <- loadBody accepted_papers_id
  unsafeCompiler $ parse_papers path

accepted_papers_list_context :: Context String
accepted_papers_list_context = field "accepted_papers_list" $ \_ -> do
  papers <- accepted_papers
  return $ format_papers_html_ul papers

process_page :: Identifier -> Item String -> Compiler (Item String)
process_page identifier page
  | identifier == "accepted.md" = do
      papers <- accepted_papers
      applyAsTemplate accepted_papers_list_context page
  | otherwise = return page

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

  -- Accepted papers.
  match (fromList [accepted_papers_id]) $
    compile copyFileCompiler

  -- This approach doesn't work because [Paper] is not writable.
  -- match (fromList [accepted_papers_id]) $ compile $ do
  --   bs <- getResourceLBS
  --   withItemBody (decode_json :: ByteString -> Compiler [Paper]) (bs :: Item ByteString)

  -- Pages of the conference website.
  match (("*.md" .||. "*.html") .&&. complement "README.md") $ do
    route $ customRoute $ toFilePath >>> (`replaceExtension` "html")
    compile page_compiler
