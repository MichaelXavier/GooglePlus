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

{-# LANGUAGE OverloadedStrings, FlexibleContexts, TypeSynonymInstances #-}
module Web.GooglePlus (getPerson,
                       getActivity,
                       getLatestActivityFeed,
                       enumActivityFeed,
                       getActivityFeed,
                       enumActivities,
                       getActivities,
                       enumPersonSearch,
                       getPersonSearch) where

import Web.GooglePlus.Types
import Web.GooglePlus.Monad

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (asks)
import           Control.Monad.Trans.Class (lift)
import           Data.Aeson (json,
                             FromJSON,
                             fromJSON,
                             parseJSON,
                             Result(..),
                             (.:),
                             (.:?),
                             Value(Object))
import           Data.Aeson.Types (typeMismatch)
import           Data.Attoparsec.Lazy (parse, eitherResult)
import           Data.ByteString (ByteString, append)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import           Data.Enumerator (Enumerator,
                                  checkContinue1,
                                  continue,
                                  Stream (Chunks),
                                  (>>==),
                                  run_,
                                  ($$))
import qualified Data.Enumerator.List as EL
import           Data.Maybe (fromMaybe)
import           Data.Text (Text, pack)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Network.HTTP.Enumerator
import           Network.HTTP.Types (Ascii, Query, QueryItem)

-- | Get a person who matches the given identifier
getPerson :: PersonID -- ^ Identifier for the person to fetch
             -> GooglePlusM (Either Text Person)
getPerson pid = genericGet pth []
  where pth = personIdPath pid


-- | Get an activity who matches the given activity ID
getActivity :: ID -- ^ Specific ID to fetch
               -> GooglePlusM (Either Text Activity)
getActivity aid = genericGet pth []
  where pth = append "/plus/v1/activities/" $ encodeUtf8 aid

-- | Get an activity who matches the given activity ID and collection to use.
-- Default page size is (20) and only fetches the first page.
-- You will receive an error from the server if the page size exceeds 100.
getLatestActivityFeed :: PersonID              -- ^ Feed owner ID
                         -> ActivityCollection -- ^ Indicates what type of feed to retrieve
                         -> Maybe Integer      -- ^ Page size. Should be between 1 and 100. Default 20.
                         -> GooglePlusM (Either Text ActivityFeed)
getLatestActivityFeed pid coll perPage = do
  feed <- getActivityFeedPage pid coll (perPageActivity perPage) Nothing
  return $ fst `fmap` feed

-- | Paginating enumerator to consume a user's activity stream. Each chunk will
-- end up being an array with a single ActivityFeed in it with 1 page of data
-- in it. This weirdness about the chunks only containing 1 element is mostly
-- to maintain the metadata available on ActivityFeed and have it available in
-- each chunk. For a more natural chunking of just Activities if you don't need
-- that additional metadata, see enumActivities. Note that this Enumerator will
-- abort if it encounters an error from the server, thus cutting the list
-- short.
enumActivityFeed :: PersonID              -- ^ Feed owner ID
                    -> ActivityCollection -- ^ Indicates what type of feed to retrieve
                    -> Maybe Integer      -- ^ Page size. Should be between 1 and 100. Defualt 20
                    -> Enumerator ActivityFeed GooglePlusM b
enumActivityFeed pid coll perPage = EL.unfoldM depaginate FirstPage
  where depaginate = depaginateActivityFeed pid coll $ perPageActivity perPage

-- | Simplified version of enumActivityFeed which retrieves all pages of an
-- activity feed and merges them into one. Note that this will not be as
-- efficient as enumActivityFeed in terms of memory/time because it collects
-- them all in memory first. Note that this should incur 1 API call per page of
-- results, so the max page size of 100 is used.
getActivityFeed :: PersonID
                   -> ActivityCollection
                   -> GooglePlusM ActivityFeed
getActivityFeed pid coll = do
  feeds <- run_ $ enumActivityFeed pid coll (Just 100) $$ EL.consume
  return $ foldl1 mergeFeeds feeds
  where mergeFeeds a ActivityFeed { activityFeedItems = is} = a { activityFeedItems = (activityFeedItems a) ++ is }

-- | Paginating enumerator yielding a Chunk for each page. Use this if you
-- don't need the feed metadata that enumActivityFeed provides.
enumActivities :: PersonID              -- ^ Feed owner ID
                  -> ActivityCollection -- ^ Indicates what type of feed to retrieve
                  -> Maybe Integer      -- ^ Page size. Should be between 1 and 100. Defualt 20
                  -> Enumerator Activity GooglePlusM b
enumActivities pid coll perPage = unfoldListM depaginate FirstPage
  where depaginate = depaginateActivities pid coll $ perPageActivity perPage

-- | Simplified version of enumActivities that fetches all the activitys of a
-- Person first, thus returning them. Note that this should incur 1 API call
-- per page of results, so the max page size of 100 is used.
getActivities :: PersonID              -- ^ Feed owner ID
                 -> ActivityCollection -- ^ Indicates what type of feed to retrieve
                 -> GooglePlusM [Activity]
getActivities pid coll = run_ $ enumActivities pid coll (Just 100) $$ EL.consume


-- | Paginating enumerator yielding a Chunk for each page. Note that this
-- Enumerator will abort if it encounters an error from the server, thus
-- cutting the list short.
enumPersonSearch :: Text             -- ^ Search string
                    -> Maybe Integer -- ^ Optional page size. Shold be between 1 and 20. Default 10
                    -> Enumerator PersonSearchResult GooglePlusM b
enumPersonSearch search perPage = unfoldListM depaginate FirstPage
  where depaginate = depaginatePersonSearch search (perPagePersonSearch perPage)

-- | Returns the full result set for a person search given a search string.
-- This interface is simpler to use but does not have the flexibility/memory
-- usage benefit of enumPersonSearch.
getPersonSearch :: Text -- ^ Search string
                -> GooglePlusM [PersonSearchResult]
getPersonSearch search = run_ $ enumPersonSearch search (Just 20) $$ EL.consume

---- Helpers

perPageActivity :: Maybe Integer -> Integer
perPageActivity = fromMaybe 20

perPagePersonSearch :: Maybe Integer -> Integer
perPagePersonSearch = fromMaybe 10

type PageToken             = Text
type PaginatedActivityFeed = (ActivityFeed, Maybe PageToken)

instance FromJSON PaginatedActivityFeed where
  parseJSON (Object v) = (,) <$> parseJSON (Object v)
                             <*> v .:? "nextPageToken"
  parseJSON v          = typeMismatch "PaginatedActivityFeed" v

data DepaginationState     = FirstPage |
                             MorePages PageToken |
                             NoMorePages

-- Exactly the same as unfoldM but takes the result of the stateful function
-- and uses it as the chunks, rather than a Chunks with a singleton list
unfoldListM :: Monad m => (s -> m (Maybe ([a], s))) -> s -> Enumerator a m b
unfoldListM f = checkContinue1 $ \loop s k -> do
	fs <- lift (f s)
	case fs of
		Nothing -> continue k
		Just (as, s') -> k (Chunks as) >>== loop s'

-- Activities Specifics

depaginateActivities :: PersonID -> ActivityCollection -> Integer -> DepaginationState -> GooglePlusM (Maybe ([Activity], DepaginationState))
depaginateActivities pid coll perPage state = (return . fmap unwrap) =<< depaginateActivityFeed pid coll perPage state
  where unwrap (feed, s) = (activityFeedItems feed, s)

depaginateActivityFeed :: PersonID -> ActivityCollection -> Integer -> DepaginationState -> GooglePlusM (Maybe (ActivityFeed, DepaginationState))
depaginateActivityFeed pid coll perPage FirstPage       = do
 page <- getFirstFeedPage pid coll perPage
 return $ paginatedState `fmap` page
depaginateActivityFeed pid coll perPage (MorePages tok) = do
 page <- getActivityFeedPage pid coll perPage $ Just tok
 return $ paginatedState `fmap` eitherMaybe page
depaginateActivityFeed _ _ _ NoMorePages                = return Nothing

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

-- PersonSearch specifics

depaginatePersonSearch :: Text -> Integer -> DepaginationState -> GooglePlusM (Maybe ([PersonSearchResult], DepaginationState))
depaginatePersonSearch search perPage state = depaginatePersonSearchResult search perPage state

type PaginatedPersonSearch = ([PersonSearchResult], Maybe PageToken)

instance FromJSON PaginatedPersonSearch where
  parseJSON (Object v) = (,) <$> v .: "items"
                             <*> v .:? "nextPageToken"
  parseJSON v          = typeMismatch "PaginatedPersonSearch" v


--TODO: needs refactor soon
depaginatePersonSearchResult :: Text -> Integer -> DepaginationState -> GooglePlusM (Maybe ([PersonSearchResult], DepaginationState))
depaginatePersonSearchResult search perPage FirstPage       = do
 page <- getFirstPersonSearchPage search perPage
 return $ paginatedState `fmap` page
depaginatePersonSearchResult search perPage (MorePages tok) = do
 page <- getPersonSearchPage search perPage $ Just tok
 return $ paginatedState `fmap` eitherMaybe page
depaginatePersonSearchResult _ _ NoMorePages                = return Nothing

paginatedState :: (a, Maybe PageToken) -> (a, DepaginationState)
paginatedState (results, token) = (results, maybe NoMorePages MorePages token)

-- TODO: refactor
getFirstPersonSearchPage :: Text -> Integer -> GooglePlusM (Maybe PaginatedPersonSearch)
getFirstPersonSearchPage search perPage = do
  page <- getPersonSearchPage search perPage Nothing
  return $ eitherMaybe page

getPersonSearchPage :: Text -> Integer -> Maybe PageToken -> GooglePlusM (Either Text PaginatedPersonSearch)
getPersonSearchPage search perPage tok = genericGet pth params
  where pth  = "/plus/v1/people"
        pageParam = BS8.pack . show $ perPage
        params = case tok of
                  Nothing -> [("maxResults", Just pageParam),
                              ("query", Just $ encodeUtf8 search)]
                  Just t  -> [("maxResults", Just pageParam),
                              ("query", Just $ encodeUtf8 search),
                              ("pageToken", Just $ encodeUtf8 t)]

-- Internals

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
genRequest auth pth q = def { host        = h,
                              path        = pth,
                              port        = 443,
                              secure      = True,
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
