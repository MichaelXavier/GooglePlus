module Main (main) where

import Test.Hspec (hspecX)

import qualified Web.GooglePlus.Testing.Types as T (specs)

main :: IO ()
main = hspecX T.specs
