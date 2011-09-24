{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Web.GooglePlus.Monad where

import Data.Text (Text)
import Control.Monad.Reader

data GooglePlusAuth = APIKey Text | OAuthToken Text

--TODO: getting tokens and using refresh tokens

data GooglePlusEnv = GooglePlusEnv { gpAuth :: GooglePlusAuth }

newtype GooglePlusM a = GooglePlusM {unGooglePlusM :: ReaderT GooglePlusEnv IO a} 
  deriving (Monad, MonadIO, MonadReader GooglePlusEnv)
