{-# LANGUAGE OverloadedStrings #-}

module FirstApp.Types.Error
  ( Error(..)
  , nonEmptyText
  ) where

import Data.Text (Text)

data Error
  = UnknownRoute
  | EmptyCommentText
  | EmptyTopic
  | DatabaseError
  deriving (Eq, Show)

nonEmptyText :: (Text -> a) -> Error -> Text -> Either Error a
nonEmptyText _ e "" = Left e
nonEmptyText c _ tx = Right (c tx)
