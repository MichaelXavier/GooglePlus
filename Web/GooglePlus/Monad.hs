--------------------------------------------------------------------
-- |
-- Module      : Web.GooglePlus.Monad
-- Description : Monadic interface for communcating with the Google+ API
-- Copyright   : (c) Michael Xavier 2011
-- License     : MIT
--
-- Maintainer: Michael Xavier <michael@michaelxavier.net>
-- Stability : provisional
-- Portability: portable
--
--------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Web.GooglePlus.Monad where

import Data.Text (Text,unpack)
import Control.Monad.Reader

-- |Represents authentication data with GooglePlus. Currently supports an OAuth
--  token or an API key
data GooglePlusAuth = APIKey Text | -- ^ Authentication using an API key
                      OAuthToken Text -- ^ Authenticate using a token obtianed via OAuth V2. Currently no way in the library to obtain refresh tokens

instance Show GooglePlusAuth where
  show (APIKey a) = show $ unpack a
  show (OAuthToken a) = show $ unpack a

--TODO: getting tokens and using refresh tokens

-- |Environment passed into requests when they are executed within a GooglePlusM
data GooglePlusEnv = GooglePlusEnv { gpAuth :: GooglePlusAuth }

-- |IO wrapper used to compose/sequence Google+ API actions. See Web.GooglePlus docs for examples
newtype GooglePlusM a = GooglePlusM {unGooglePlusM :: ReaderT GooglePlusEnv IO a} 
  deriving (Monad, MonadIO, MonadReader GooglePlusEnv)
