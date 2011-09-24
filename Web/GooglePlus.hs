{-# LANGUAGE OverloadedStrings #-}
module Web.GooglePlus (getPerson, getActivity, getActivityFeed) where

import Web.GooglePlus.Types
import Web.GooglePlus.Monad

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (asks)
import           Data.Aeson (json, FromJSON, fromJSON, Result(..))
import           Data.Attoparsec.Lazy (parse, eitherResult)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.ByteString (append)
import           Data.Text (Text, pack)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Network.HTTP.Enumerator
import           Network.HTTP.Types (Ascii, Query, QueryItem)

getPerson :: PersonID -> GooglePlusM (Either Text Person)
getPerson pid = genericGet pth []
  where pth = personIdPath pid


getActivity :: ID -> GooglePlusM (Either Text Activity)
getActivity aid = genericGet pth []
  where pth = append "/plus/v1/activities/" $ encodeUtf8 aid

--TODO: pagetoken
getActivityFeed :: PersonID -> ActivityCollection -> GooglePlusM (Either Text ActivityFeed)
getActivityFeed pid coll = genericGet pth []
  where pth  = append pidP actP
        pidP = personIdPath pid
        actP = append "/activities/" $ collectionPath coll

---- Helpers

genericGet :: FromJSON a => Ascii -> Query -> GooglePlusM (Either Text a)
genericGet pth qs = withEnv $ \auth -> do
  resp <- doGet auth pth []
  return $ handleResponse resp

collectionPath :: ActivityCollection -> ByteString
collectionPath PublicCollection = "public"

personIdPath :: PersonID -> ByteString
personIdPath (PersonID i) = append "/plus/v1/people/" $ encodeUtf8 i
personIdPath Me           = "/plus/v1/people/me"

doGet :: GooglePlusAuth -> Ascii -> Query -> GooglePlusM (Int, LBS.ByteString)
doGet auth pth q = liftIO $ withManager $ \manager -> do
  Response { statusCode = c, responseBody = b} <- httpLbsRedirect req manager
  return (c, b)
  where req = genRequest auth pth q

--TODO: handle oauth
genRequest :: GooglePlusAuth -> Ascii -> Query -> Request m
genRequest auth pth q = def { host = h,
                              path = pth,
                              port = 443,
                              secure = True,
                              queryString = q' }
  where h     = "www.googleapis.com"
        authq = authParam auth
        q'    = authq:q


authParam :: GooglePlusAuth -> QueryItem
authParam (APIKey key)     = ("key", Just $ encodeUtf8 key)
authParam (OAuthToken tok) = ("access_token", Just $ encodeUtf8 tok)

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
