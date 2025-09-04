{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll

main :: IO ()
main = hakyll $ do
    match "index.html" $ do
        route   idRoutee
        compile copyFileCompiler
