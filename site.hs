{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow ((>>>))
import Control.Monad (forM)
import Data.Function ((&))
import Data.List (stripPrefix)
import Data.Maybe (fromJust)
import Data.Monoid (mappend)
import Data.String (fromString)
import System.FilePath (makeRelative, replaceExtension, (</>))
import Hakyll

base :: FilePath
base = "hidden"

base_to :: FilePath -> FilePath
base_to = (base </>)

base_from :: FilePath -> FilePath
base_from = makeRelative base

-- Used to build the navigation bar.
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
      return $ path & base_from
  ]

navigation_id :: Identifier
navigation_id = "templates/navigation.html"

navigation_context :: Context String
navigation_context = defaultContext <> field "navigation" (\_ -> loadBody navigation_id)

main :: IO ()
main = hakyll $ do
  -- Placeholder "under construction".
  match "placeholder/**" $ do
    route idRoute
    compile copyFileCompiler

  -- README for website editors.
  -- Delete from generation once everyone has access to the sources.
  match "README.md" $ do
    route $ customRoute $ toFilePath >>> (`replaceExtension` "html") >>> base_to
    compile pandocCompiler

  -- Files that should just be copied over.
  match ("files/**" .||. "images/**" .||. "abstracts/**" .||. "slides/**") $ do
    route $ customRoute $ toFilePath >>> base_to
    compile copyFileCompiler

  -- Templates.
  match "templates/default.html" $ compile templateBodyCompiler

  -- Navigation bar.
  -- Not actually a template.
  -- Made available for templates via navigation_context.
  match (fromList [navigation_id]) $ compile $ do
    metadata <- getUnderlying >>= getMetadata
    body <- getResourceBody
    let Just navigation_ids = lookupStringList "navigation_ids" metadata
    let context = listField "navigation_items" nav_item_context $ forM navigation_ids $ fromFilePath >>> makeItem
    applyAsTemplate context body

  -- Pages of the conference website.
  match (("*.md" .||. "*.html") .&&. complement "README.md") $ do
    route $ customRoute $ toFilePath >>> (`replaceExtension` "html") >>> base_to
    compile $ pandocCompiler >>= loadAndApplyTemplate "templates/default.html" navigation_context
