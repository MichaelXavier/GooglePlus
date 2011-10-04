--------------------------------------------------------------------
-- |
-- Module      : Web.GooglePlus
-- Description : Toplevel module for the Google+ API
-- Copyright   : (c) Michael Xavier 2011
-- License     : MIT
--
-- Maintainer: Michael Xavier <michael@michaelxavier.net>
-- Stability : provisional
-- Portability: portable
--
-- Toplevel module for the Google+ API operating in the GooglePlusM Monad. 
-- Currently covers the (very) limited, read-only API that Google has exposed
-- in v1 of the Google+ API
-- 
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import Web.GooglePlus
-- > import Web.GooglePlus.Monad
-- > import Web.GooglePlus.Types
-- > import Control.Monad.Reader
-- > import Data.Text (unpack)
-- > 
-- > doStuff :: GooglePlusM ()
-- > doStuff = do
-- >   Right person <- getPerson Me
-- >   Right feed   <- getLatestActivityFeed Me PublicCollection
-- >   -- ...
-- >   return ()
-- > 
-- > main :: IO ()
-- > main = do
-- >   runReaderT (unGooglePlusM doStuff) env
-- >   where env  = GooglePlusEnv { gpAuth = APIKey "MYKEY" }
-- 
--------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Web.GooglePlus (getPerson,
                       getActivity,
                       getLatestActivityFeed,
                       enumActivityFeed) where

import Web.GooglePlus.Types
import Web.GooglePlus.Monad

import           Control.Failure (Failure)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (asks)
import           Data.Aeson (json,
                             FromJSON,
                             fromJSON,
                             Result(..),
                             Object(..),
                             Value(Object))
import           Data.Attoparsec.Lazy (parse, eitherResult)
import           Data.ByteString (ByteString, append)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import           Data.Enumerator (Enumerator,
                                  joinI,
                                  ($$))
import qualified Data.Enumerator.List as EL
import           Data.Maybe (fromMaybe)
import           Data.Text (Text, pack)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Network.HTTP.Enumerator
import           Network.HTTP.Types (Ascii, Query, QueryItem)

-- | Get a person who matches the given identifier
getPerson :: PersonID -> GooglePlusM (Either Text Person)
getPerson pid = genericGet pth []
  where pth = personIdPath pid


-- | Get an activity who matches the given activity ID
getActivity :: ID -> GooglePlusM (Either Text Activity)
getActivity aid = genericGet pth []
  where pth = append "/plus/v1/activities/" $ encodeUtf8 aid

--TODO: pagetoken, pagination, limits, enumerator?

-- | Get an activity who matches the given activity ID and collection to use.
-- Currently uses the default page size (20) and only fetches the first page.
-- You will receive an error from the server if the page size exceeds 100.
getLatestActivityFeed :: PersonID -> ActivityCollection -> Maybe Integer -> GooglePlusM (Either Text ActivityFeed)
getLatestActivityFeed pid coll perPage = do
  feed <- getActivityFeedPage pid coll (perPage' perPage) Nothing
  return $ fst `fmap` feed

enumActivityFeed :: PersonID -> ActivityCollection -> Maybe Integer -> Enumerator ActivityFeed GooglePlusM b
enumActivityFeed pid coll perPage = EL.unfoldM depaginate FirstPage
  where depaginate = depaginateActivityFeed pid coll $ perPage' perPage

-- TODO: simplified, more semantic version, enumActivities

---- Helpers

perPage' :: Maybe Integer -> Integer
perPage' = fromMaybe defaultPageSize

defaultPageSize :: Integer
defaultPageSize = 20

type PageToken             = Text
type PaginatedActivityFeed = (ActivityFeed, Maybe PageToken)
data DepaginationState     = FirstPage |
                             MorePages PageToken |
                             NoMorePages

depaginateActivityFeed :: PersonID -> ActivityCollection -> Integer -> DepaginationState -> GooglePlusM (Maybe (ActivityFeed, DepaginationState))
depaginateActivityFeed pid coll perPage FirstPage       = do
 page <- getFirstFeedPage pid coll perPage
 return $ paginatedFeedState `fmap` page
depaginateActivityFeed pid coll perPage (MorePages tok) = do
 page <- getActivityFeedPage pid coll perPage $ Just tok
 return $ paginatedFeedState `fmap` eitherMaybe page
depaginateActivityFeed _ _ _ NoMorePages                = return Nothing

paginatedFeedState :: PaginatedActivityFeed -> (ActivityFeed, DepaginationState)
paginatedFeedState (feed, token) = (feed, maybe NoMorePages MorePages token)

getFirstFeedPage :: PersonID -> ActivityCollection -> Integer -> GooglePlusM (Maybe PaginatedActivityFeed)
getFirstFeedPage pid coll perPage = do
  page <- getActivityFeedPage pid coll perPage Nothing
  return $ eitherMaybe page

getActivityFeedPage :: PersonID -> ActivityCollection -> Integer -> Maybe PageToken -> GooglePlusM (Either Text PaginatedActivityFeed)
getActivityFeedPage pid coll perPage tok = genericGet pth params
  where pth  = append pidP actP
        pidP = personIdPath pid
        actP = append "/activities/" $ collectionPath coll
        pageParam = BS8.pack . show $ perPage
        params = case tok of
                  Nothing -> [("maxResults", Just pageParam)]
                  Just t  -> [("maxResults", Just pageParam), ("pageToken", Just $ encodeUtf8 t)]

eitherMaybe :: Either a b -> Maybe b
eitherMaybe (Left _)  = Nothing
eitherMaybe (Right x) = Just x

genericGet :: FromJSON a => Ascii -> Query -> GooglePlusM (Either Text a)
genericGet pth qs = withEnv $ \auth -> do
  resp <- doGet auth pth qs
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
                    Error e   -> Left e
        parsed  = eitherResult $ parse json str
handleResponse (_, str) = Left $ decodeUtf8 . BS.concat . LBS.toChunks $ str

packLeft :: Either String a -> Either Text a
packLeft (Right x)  = Right x
packLeft (Left str) = Left $ pack str

withEnv :: (GooglePlusAuth -> GooglePlusM a) -> GooglePlusM a
withEnv fn = fn =<< asks gpAuth 

--TODO: not needed?
left =. right = \step_i -> joinI $ left $$ right step_i
