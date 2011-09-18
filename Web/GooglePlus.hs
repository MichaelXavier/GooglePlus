{-# LANGUAGE OverloadedStrings #-}
module Web.GooglePlus (getPerson) where

import Web.GooglePlus.Types
import Web.GooglePlus.Monad

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (asks)
import           Data.Aeson (json, FromJSON, fromJSON, Result(..))
import           Data.Attoparsec.Lazy (parse, eitherResult)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.ByteString (append)
import           Data.Text (Text, pack)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Network.HTTP.Enumerator
import           Network.HTTP.Types (Ascii, Query, QueryItem)

getPerson :: PersonID -> GooglePlusM (Either Text Person)
getPerson pid = withEnv $ \auth -> do
  resp <- doGet auth pth []
  return $ handleResponse resp
  where pth = case pid of
                PersonID i -> append "/plus/v1/people/" $ encodeUtf8 i
                Me         -> "/plus/v1/people/me"

doGet :: GooglePlusAuth -> Ascii -> Query -> GooglePlusM (Int, LBS.ByteString)
doGet auth pth q = liftIO $ withManager $ \manager -> do
  Response { statusCode = c, responseBody = b} <- httpLbsRedirect req manager
  return (c, b)
  where req = genRequest auth pth q

--TODO: handle oauth
genRequest :: GooglePlusAuth -> Ascii -> Query -> Request m
genRequest (APIKey key) pth q = def { host = h,
                                      path = pth,
                                      port = 443,
                                      secure = True,
                                      queryString = q' }
  where h  = "www.googleapis.com"
        q' = ("key", Just $ encodeUtf8 key):q

handleResponse :: FromJSON a => (Int, LBS.ByteString) -> Either Text a
handleResponse (200, str) = packLeft $ fjson =<< parsed
  where fjson v = case fromJSON v of
                    Success a -> Right a
                    Error str -> Left str
        parsed  = eitherResult $ parse json str
handleResponse (_, str) = Left $ decodeUtf8 . BS.concat . LBS.toChunks $ str

packLeft :: Either String a -> Either Text a
packLeft (Right x)  = Right x
packLeft (Left str) = Left $ pack str

withEnv :: (GooglePlusAuth -> GooglePlusM a) -> GooglePlusM a
withEnv fn = fn =<< asks gpAuth 
