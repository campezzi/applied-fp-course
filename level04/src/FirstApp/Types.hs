{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module FirstApp.Types
  ( Error(..)
  , RqType(..)
  , ContentType(..)
  , Topic
  , CommentText
  , Comment(..)
  , mkTopic
  , getTopic
  , mkCommentText
  , getCommentText
  , renderContentType
  , fromDbComment
  , fromDbTopic
  ) where

import GHC.Generics (Generic)

import Data.ByteString (ByteString)
import Data.Char (toLower)
import Data.Text (Text)

import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)

import Data.Aeson (ToJSON(toJSON))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A

import Data.Time (UTCTime)

import FirstApp.DB.Types
       (DBComment, DBTopic, dbCommentBody, dbCommentId, dbCommentTime,
        dbCommentTopic, dbTopicName)
import FirstApp.Types.CommentText
       (CommentText, getCommentText, mkCommentText)
import FirstApp.Types.Error
       (Error(DatabaseError, EmptyCommentText, EmptyTopic, UnknownRoute))
import FirstApp.Types.Topic (Topic, getTopic, mkTopic)

-- This is the `Comment` record that we will be sending to users, it's a simple
-- record type, containing an `Int`, `Topic`, `CommentText`, and `UTCTime`.
-- However notice that we've also derived the `Generic` type class instance as
-- well. This saves us some effort when it comes to creating encoding/decoding
-- instances. Since our types are all simple types at the end of the day, we're
-- able to let GHC do the work.
newtype CommentId =
  CommentId Int
  deriving (Eq, Show, ToJSON)

data Comment = Comment
  { commentId :: CommentId
  , commentTopic :: Topic
  , commentBody :: CommentText
  , commentTime :: UTCTime
  } deriving (Show, Generic)

-- Strip the prefix (which may fail if the prefix isn't present), fall
-- back to the original label if need be.
-- | modFieldLabel
-- >>> modFieldLabel "commentId"
-- "id"
-- >>> modFieldLabel "topic"
-- "topic"
-- >>> modFieldLabel ""
-- ""
modFieldLabel :: String -> String
modFieldLabel fieldName =
  maybe fieldName firstLetterToLower (stripPrefix "comment" fieldName)

firstLetterToLower :: String -> String
firstLetterToLower (firstLetter:rest) = toLower firstLetter : rest
firstLetterToLower "" = ""

instance ToJSON Comment
  -- This is one place where we can take advantage of our `Generic` instance.
  -- Aeson already has the encoding functions written for anything that
  -- implements the `Generic` typeclass. So we don't have to write our encoding,
  -- we ask Aeson to construct it for us.
                                          where
  toEncoding = A.genericToEncoding opts
      -- These options let us make some minor adjustments to how Aeson treats
      -- our type. Our only adjustment is to alter the field names a little, to
      -- remove the 'comment' prefix and use an Aeson function to handle the
      -- rest of the name. This accepts any 'String -> String' function but it's
      -- wise to keep the modifications simple.
    where
      opts = A.defaultOptions {A.fieldLabelModifier = modFieldLabel}

-- For safety we take our stored `DbComment` and try to construct a `Comment`
-- that we would be okay with showing someone. However unlikely it may be, this
-- is a nice method for separating out the back and front end of a web app and
-- providing greater guarantees about data cleanliness.
fromDbComment :: DBComment -> Either Error Comment
fromDbComment dbComment =
  case comment of
    Left _ -> Left DatabaseError
    Right _ -> comment
  where
    comment =
      Comment (CommentId $ dbCommentId dbComment) <$>
      mkTopic (dbCommentTopic dbComment) <*>
      mkCommentText (dbCommentBody dbComment) <*>
      Right (dbCommentTime dbComment)

fromDbTopic :: DBTopic -> Either Error Topic
fromDbTopic dbTopic =
  case topic of
    Left _ -> Left DatabaseError
    Right _ -> topic
  where
    topic = mkTopic (dbTopicName dbTopic)

data RqType
  = AddRq Topic
          CommentText
  | ViewRq Topic
  | ListRq

data ContentType
  = PlainText
  | JSON

renderContentType :: ContentType -> ByteString
renderContentType PlainText = "text/plain"
renderContentType JSON = "application/json"
