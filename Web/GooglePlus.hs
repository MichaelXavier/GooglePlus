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

{-# LANGUAGE OverloadedStrings, FlexibleContexts, TypeSynonymInstances, FlexibleInstances #-}
module Web.GooglePlus (getPerson,
                       getActivity,
                       getComment,
                       getLatestActivityFeed,
                       enumActivityFeed,
                       getActivityFeed,
                       enumActivities,
                       getActivities,
                       enumPersonSearch,
                       getPersonSearch,
                       enumPeopleByActivity,
                       getPeopleByActivity,
                       enumActivitySearch,
                       getActivitySearch,
                       enumComments,
                       getComments,
                       SearchOrderBy(..),
                       ActivityCollection(..),
                       ListByActivityCollection(..)) where

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


-- | Get an activity which matches the given activity ID
getActivity :: ID -- ^ Specific ID to fetch
               -> GooglePlusM (Either Text Activity)
getActivity aid = genericGet pth []
  where pth = "/plus/v1/activities/" `append` encodeUtf8 aid

-- | Get a comment which matches the given comment ID
getComment :: ID -- ^ Specific ID to fetch
              -> GooglePlusM (Either Text Comment)
getComment cid = genericGet pth []
  where pth = "/plus/v1/comments/" `append` encodeUtf8 cid

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
  where mergeFeeds a ActivityFeed { activityFeedItems = is} = a { activityFeedItems = activityFeedItems a ++ is }

-- | Paginating enumerator yielding a Chunk for each page. Use this if you
-- don't need the feed metadata that enumActivityFeed provides.
enumActivities :: PersonID              -- ^ Feed owner ID
                  -> ActivityCollection -- ^ Indicates what type of feed to retrieve
                  -> Maybe Integer      -- ^ Page size. Should be between 1 and 100. Defualt 20
                  -> Enumerator Activity GooglePlusM b
enumActivities pid coll perPage = simpleDepaginator depaginate
  where depaginate = simpleDepaginationStep perPage' pth params
        pth        = pidP `append` actP
        actP       = "/activities/" `append` collectionPath coll
        pidP       = personIdPath pid
        params     = []
        perPage'   = perPageActivity perPage

-- | Simplified version of enumActivities that fetches all the activitys of a
-- Person first, thus returning them. Note that this should incur 1 API call
-- per page of results, so the max page size of 100 is used.
getActivities :: PersonID              -- ^ Feed owner ID
                 -> ActivityCollection -- ^ Indicates what type of feed to retrieve
                 -> GooglePlusM [Activity]
getActivities pid coll = run_ $ enumActivities pid coll (Just 100) $$ EL.consume


-- | Search for a member of Google+. Paginating enumerator yielding a Chunk for
-- each page. Note that this Enumerator will abort if it encounters an error
-- from the server, thus cutting the list short.
enumPersonSearch :: Text             -- ^ Search string
                    -> Maybe Integer -- ^ Optional page size. Shold be between 1 and 20. Default 10
                    -> Enumerator PersonSearchResult GooglePlusM b
enumPersonSearch search perPage = simpleDepaginator depaginate
  where depaginate = simpleDepaginationStep perPage' pth params
        pth        = "/plus/v1/people"
        params     = [("query", Just $ encodeUtf8 search)]
        perPage'   = perPageSearch perPage

-- | Returns the full result set for a person search given a search string.
-- This interface is simpler to use but does not have the flexibility/memory
-- usage benefit of enumPersonSearch.
getPersonSearch :: Text -- ^ Search string
                -> GooglePlusM [PersonSearchResult]
getPersonSearch search = run_ $ enumPersonSearch search (Just 20) $$ EL.consume

-- | Find people associated with a particular Activity. Paginating enumerator
-- yielding a Chunk for each page. Paginating enumerator yielding a Chunk for
-- each page. Note that this Enumerator will abort if it encounters an error
-- from the server, thus cutting the list short.
enumPeopleByActivity :: ID                          -- ^ Activity ID
                        -> ListByActivityCollection -- ^ Indicates which collection of people to list
                        -> Maybe Integer            -- ^ Optional page size. Should be between 1 and 100. Default 20.
                        -> Enumerator Person GooglePlusM b
enumPeopleByActivity aid coll perPage = simpleDepaginator depaginate
  where depaginate      = simpleDepaginationStep perPage' pth params
        pth             = "/plus/v1/activities/" `append` encodeUtf8 aid `append` peopleP `append` collP coll
        peopleP         = "/people/"
        collP PlusOners = "plusoners"
        collP Resharers = "resharers"
        params          = []
        perPage'        = perPageActivity perPage

-- | Returns the full result set for a person search given a search string.
-- This interface is simpler to use but does not have the flexibility/memory
-- usage benefit of enumPeopleByActivity.
getPeopleByActivity :: ID                          -- ^ Activity ID
                       -> ListByActivityCollection -- ^ Indicates which collection of people to list
                       -> GooglePlusM [Person]
getPeopleByActivity aid coll = run_ $ enumPeopleByActivity aid coll (Just 100) $$ EL.consume

-- | Search for an activity on Google+. Paginating enumerator yielding a Chunk
-- for each page. Note that this Enumerator will abort if it encounters an error
-- from the server, thus cutting the list short.
enumActivitySearch :: Text             -- ^ Search string
                      -> SearchOrderBy -- ^ Order of search results
                      -> Maybe Integer -- ^ Optional page size. Shold be between 1 and 20. Default 10
                      -> Enumerator Activity GooglePlusM b
enumActivitySearch search orderBy perPage = simpleDepaginator depaginate
  where depaginate        = simpleDepaginationStep perPage' pth params
        pth               = "/plus/v1/activities"
        params            = [("query", Just $ encodeUtf8 search),
                             ("orderBy", Just $ orderParam orderBy)]
        orderParam Best   = "best"
        orderParam Recent = "recent"
        perPage'          = perPageSearch perPage

-- | Returns the full result set for an activity search given a search string.
-- This interface is simpler to use but does not have the flexibility/memory
-- usage benefit of enumActivitySearch.
getActivitySearch :: Text             -- ^ Search string
                     -> SearchOrderBy -- ^ Order of search results
                     -> GooglePlusM [Activity]
getActivitySearch search orderBy = run_ $ enumActivitySearch search orderBy (Just 20) $$ EL.consume

-- | Find comments for an activity on Google+. Paginating enumerator yielding a
-- Chunk for each page. Note that this Enumerator will abort if it encounters
-- an error from the server, thus cutting the list short.
enumComments :: ID               -- ^ Activity ID
                -> Maybe Integer -- ^ Optional page size. Should be between 1 and 100. Default 20
                -> Enumerator Comment GooglePlusM b
enumComments aid perPage = simpleDepaginator depaginate
  where depaginate = simpleDepaginationStep perPage' pth params
        pth        = "/plus/v1/activities/" `append` encodeUtf8 aid `append` "/comments"
        params     = []
        perPage'   = perPageActivity perPage

-- | Returns the full result set for an activity's comments. This interface is
-- simpler to use but does not have the flexibility/memory usage benefit of
-- enumComments.
getComments :: ID -- ^ Activity ID
              -> GooglePlusM [Comment]
getComments aid = run_ $ enumComments aid (Just 100) $$ EL.consume

-- | Specifies the type of Activities to get in an Activity listing. Currently
-- the API only allows public.
data ActivityCollection = PublicCollection deriving (Show, Eq)

data ListByActivityCollection = PlusOners | -- ^ List of people who have +1ed an activity
                                Resharers   -- ^ List of people who have reshared an activity
                                deriving (Show, Eq)

data SearchOrderBy = Best | -- ^ Sort by relevance to the to the user, most relevant first
                     Recent -- ^ Sort by most recent results first
                     deriving (Show, Eq)

---- Helpers

simpleDepaginator  :: Monad m => (DepaginationState -> m (Maybe ([a], DepaginationState)))
                                 -> Enumerator a m b
simpleDepaginator depaginate = unfoldListM depaginate FirstPage

perPageActivity :: Maybe Integer
                   -> Integer
perPageActivity = fromMaybe 20

perPageSearch :: Maybe Integer
                 -> Integer
perPageSearch = fromMaybe 10

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
unfoldListM :: Monad m => (s -> m (Maybe ([a], s)))
                          -> s
                          -> Enumerator a m b
unfoldListM f = checkContinue1 $ \loop s k -> do
	fs <- lift (f s)
	case fs of
		Nothing -> continue k
		Just (as, s') -> k (Chunks as) >>== loop s'

simpleGetFirstPage :: FromJSON a => Integer
                                    -> Ascii
                                    -> Query
                                    -> GooglePlusM (Maybe (PaginatedResource a))
simpleGetFirstPage perPage = simpleGetPage perPage Nothing

simpleGetPage :: FromJSON a => Integer
                            -> Maybe PageToken 
                            -> Ascii 
                            -> Query 
                            -> GooglePlusM (Maybe (PaginatedResource a))
simpleGetPage perPage tok pth params = do
  page <- genericGet pth $ params ++ pageParams
  return $ eitherMaybe page
  where pageParam    = BS8.pack . show $ perPage
        pageParams   = case tok of
                         Nothing -> [("maxResults", Just pageParam)]
                         Just t  -> [("maxResults", Just pageParam), ("pageToken", Just $ encodeUtf8 t)]

simpleDepaginationStep :: FromJSON a => Integer
                                     -> Ascii 
                                     -> Query 
                                     -> DepaginationState 
                                     -> GooglePlusM (Maybe ([a], DepaginationState))
simpleDepaginationStep perPage pth params FirstPage       = (return . fmap paginatedState) =<< simpleGetFirstPage perPage pth params
simpleDepaginationStep perPage pth params (MorePages tok) = (return . fmap paginatedState) =<< simpleGetPage perPage (Just tok) pth params
simpleDepaginationStep _ _ _ NoMorePages = return Nothing

-- Activities Specifics

depaginateActivityFeed :: PersonID
                          -> ActivityCollection
                          -> Integer
                          -> DepaginationState
                          -> GooglePlusM (Maybe (ActivityFeed, DepaginationState))
depaginateActivityFeed pid coll perPage FirstPage       = do
 page <- getFirstFeedPage pid coll perPage
 return $ paginatedState `fmap` page
depaginateActivityFeed pid coll perPage (MorePages tok) = do
 page <- getActivityFeedPage pid coll perPage $ Just tok
 return $ paginatedState `fmap` eitherMaybe page
depaginateActivityFeed _ _ _ NoMorePages                = return Nothing

getFirstFeedPage :: PersonID
                    -> ActivityCollection
                    -> Integer
                    -> GooglePlusM (Maybe PaginatedActivityFeed)
getFirstFeedPage pid coll perPage = do
  page <- getActivityFeedPage pid coll perPage Nothing
  return $ eitherMaybe page

getActivityFeedPage :: PersonID
                       -> ActivityCollection
                       -> Integer
                       -> Maybe PageToken
                       -> GooglePlusM (Either Text PaginatedActivityFeed)
getActivityFeedPage pid coll perPage tok = genericGet pth params
  where pth  = pidP `append` actP
        pidP = personIdPath pid
        actP = "/activities/" `append` collectionPath coll
        pageParam = BS8.pack . show $ perPage
        params = case tok of
                  Nothing -> [("maxResults", Just pageParam)]
                  Just t  -> [("maxResults", Just pageParam), ("pageToken", Just $ encodeUtf8 t)]

type PaginatedResource a = ([a], Maybe PageToken)

instance FromJSON a => FromJSON (PaginatedResource a) where
  parseJSON (Object v) = (,) <$> v .: "items"
                            <*> v .:? "nextPageToken"
  parseJSON v          = typeMismatch "PaginatedResource" v

paginatedState :: (a, Maybe PageToken)
                  -> (a, DepaginationState)
paginatedState (results, token) = (results, maybe NoMorePages MorePages token)

-- Internals

eitherMaybe :: Either a b
               -> Maybe b
eitherMaybe (Left _)  = Nothing
eitherMaybe (Right x) = Just x

genericGet :: FromJSON a => Ascii
                            -> Query
                            -> GooglePlusM (Either Text a)
genericGet pth qs = withEnv $ \auth -> return . handleResponse =<< doGet auth pth qs

collectionPath :: ActivityCollection
                  -> ByteString
collectionPath PublicCollection = "public"

personIdPath :: PersonID
                -> ByteString
personIdPath (PersonID i) = "/plus/v1/people/" `append` encodeUtf8 i
personIdPath Me           = "/plus/v1/people/me"

doGet :: GooglePlusAuth
         -> Ascii
         -> Query
         -> GooglePlusM (Int, LBS.ByteString)
doGet auth pth q = liftIO $ withManager $ \manager -> do
  Response { statusCode = c, responseBody = b} <- httpLbsRedirect req manager
  return (c, b)
  where req = genRequest auth pth q

genRequest :: GooglePlusAuth
              -> Ascii
              -> Query
              -> Request m
genRequest auth pth q = def { host        = h,
                              path        = pth,
                              port        = 443,
                              secure      = True,
                              queryString = q' }
  where h     = "www.googleapis.com"
        authq = authParam auth
        q'    = authq:q

authParam :: GooglePlusAuth
             -> QueryItem
authParam (APIKey key)     = ("key", Just $ encodeUtf8 key)
authParam (OAuthToken tok) = ("access_token", Just $ encodeUtf8 tok)

handleResponse :: FromJSON a => (Int, LBS.ByteString)
                                -> Either Text a
handleResponse (200, str) = packLeft $ fjson =<< parsed
  where fjson v = case fromJSON v of
                    Success a -> Right a
                    Error e   -> Left e
        parsed  = eitherResult $ parse json str
handleResponse (_, str) = Left $ decodeUtf8 . BS.concat . LBS.toChunks $ str

packLeft :: Either String a
            -> Either Text a
packLeft (Right x)  = Right x
packLeft (Left str) = Left $ pack str

withEnv :: (GooglePlusAuth -> GooglePlusM a)
           -> GooglePlusM a
withEnv fn = fn =<< asks gpAuth 
