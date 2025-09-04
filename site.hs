{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll

main :: IO ()
main = hakyll $ do
    match "index.html2" $ do
        route   idRoute
        compile copyFileCompiler
