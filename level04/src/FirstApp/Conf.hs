{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module FirstApp.Conf
  ( Conf(..)
  , firstAppConfig
  ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data Conf = Conf
  { dbFilePath :: FilePath
  , serverPort :: Int
  } deriving (Show, Generic)

instance FromJSON Conf

firstAppConfig :: Conf
firstAppConfig = Conf "app_db.db" 3000
