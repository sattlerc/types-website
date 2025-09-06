{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow ((>>>))
import Data.List (stripPrefix)
import Data.Monoid (mappend)
import Data.Maybe (fromJust)
import System.FilePath ((</>), makeRelative, replaceExtension)
import Hakyll

base :: FilePath
base = "hidden"

base_to :: FilePath -> FilePath
base_to = (base </>)

base_from :: FilePath -> FilePath
base_from = makeRelative base

main :: IO ()
main = hakyll $ do
  match "placeholder/**" $ do
    route idRoute
    compile copyFileCompiler

  match ("files/**" .||. "images/**" .||. "abstracts/**" .||. "slides/**") $ do
    route $ customRoute $ toFilePath >>> base_to
    compile copyFileCompiler
  
  match "templates/*" $ compile templateBodyCompiler

  match "*.md" $ do
    route $ customRoute $ toFilePath >>> (`replaceExtension` "html") >>> base_to
    compile $ do
      id <- pandocCompiler
      loadAndApplyTemplate "templates/default.html" defaultContext id
