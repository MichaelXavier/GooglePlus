{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Web.GooglePlus.Types

import Data.ByteString (append)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Enumerator
import Network.HTTP.Types (Ascii, Query, QueryItem)


main :: IO ()
main = do getPerson env pid
          return ()
  where env = GooglePlusEnv { apiKey = ak }
        pid = PersonID "108189587050871927619"

data GooglePlusEnv = GooglePlusEnv { apiKey :: Text }


getPerson :: GooglePlusEnv -> PersonID -> IO Person
getPerson env pid = withManager $ \manager -> do
  --putStrLn . show . path $ req
  Response { statusCode = c, responseBody = b} <- httpLbsRedirect req manager
  putStrLn . show $ c
  putStrLn . show $ b
  return Person --TODO
  where req = genRequest env q pth
        pth = case pid of
                PersonID i -> append "/plus/v1/people/" $ encodeUtf8 i
                Me         -> "/plus/v1/people/me"
        q   = []

genRequest :: GooglePlusEnv -> Query -> Ascii -> Request m
genRequest GooglePlusEnv { apiKey = key } q pth = def { host = h,
                                                        path = pth,
                                                        port = 443,
                                                        secure = True,
                                                        queryString = q' }
  where h = "www.googleapis.com"
        q' = ("key", Just $ encodeUtf8 key):q

ak :: Text
ak = "SUPERDUPERSECRET"
