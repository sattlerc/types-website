{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow ((>>>))
import Data.List (stripPrefix)
import Data.Monoid (mappend)
import Data.Maybe (fromJust)
import Hakyll

main :: IO ()
main = hakyll $ do
  match "pages/*" $ do
    route $ customRoute $ toFilePath >>> stripPrefix "pages/" >>> fromJust >>> ("hidden/" ++)
    compile copyFileCompiler

  match "index.html" $ do
    route idRoute
    compile copyFileCompiler
