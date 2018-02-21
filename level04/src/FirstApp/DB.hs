{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module FirstApp.DB
  ( FirstAppDB(FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import Data.Text (Text, pack)
import qualified Data.Text as Text

import Data.Time (getCurrentTime)

import Database.SQLite.Simple
       (Connection, FromRow, Query(Query), fromRow)
import qualified Database.SQLite.Simple as Sql

import qualified Database.SQLite.SimpleErrors as Sql
import Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import FirstApp.Types
       (Comment, CommentText, Error, Topic, fromDbComment, fromDbTopic,
        getCommentText, getTopic, mkTopic)

-- ------------------------------------------------------------------------|
-- You'll need the documentation for sqlite-simple ready for this section! |
-- ------------------------------------------------------------------------|
-- We have a data type to simplify passing around the information we need to run
-- our database queries. This also allows things to change over time without
-- having to rewrite all of the functions that need to interact with DB related
-- things in different ways.
--
-- To help with that, we create a new data type that can hold our `Connection`
-- for us, and allows it to be expanded later if we need to
newtype FirstAppDB = FirstAppDB
  { dbConn :: Connection
  }

-- Quick helper to pull the connection and close it down.
closeDB :: FirstAppDB -> IO ()
closeDB = Sql.close . dbConn

-- Given a `FilePath` to our SQLite DB file, initialise the database and ensure
-- our Table is there by running a query to create it, if it doesn't exist
-- already.
initDB :: FilePath -> IO (Either SQLiteResponse FirstAppDB)
initDB fp = do
  conn <- Sql.open fp
  Sql.execute_ conn createTableQ
  return $ pure $ FirstAppDB conn
  where
    createTableQ =
      "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time TEXT)"

-- Note that we don't store the `Comment` in the DB, it is the type we build
-- to send to the outside world. We will be loading our `DbComment` type from
-- the FirstApp.DB.Types module before converting trying to convert it to a
-- `Comment`.
--
-- To go from a DbComment to a Comment, we need to use ``fromDbComment`` that is
-- defined in FirstApp.Types.
--
-- HINT: You can use '?' or named place-holders as query parameters. Have a look
-- at the section on parameter substitution in sqlite-simple's documentation.
getComments :: FirstAppDB -> Topic -> IO (Either Error [Comment])
getComments db topic = do
  results <- Sql.query (dbConn db) sql [getTopic topic]
  return $ pure $ foldr fromDbCommentIgnoringErrors [] results
  where
    sql = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"
    fromDbCommentIgnoringErrors comment acc =
      case fromDbComment comment of
        Left _ -> acc -- ignoring errors on individual records
        Right c -> c : acc
  -- There are several possible implementations of this function. Paritcularly
  -- there may be a trade-off between deciding to throw an Error if a DbComment
  -- cannot be converted to a Comment, or simply ignoring any DbComment that is
  -- not valid.

addCommentToTopic :: FirstAppDB -> Topic -> CommentText -> IO (Either Error ())
addCommentToTopic db topic commentText = do
  currentTime <- getCurrentTime
  Sql.execute
    (dbConn db)
    sql
    (getTopic topic, getCommentText commentText, currentTime)
  return $ pure ()
  where
    sql = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"

getTopics :: FirstAppDB -> IO (Either Error [Topic])
getTopics db = do
  results <- Sql.query_ (dbConn db) sql
  return $ pure $ foldr fromDbTopicIgnoringErrors [] results
  where
    sql = "SELECT DISTINCT topic FROM comments"
    fromDbTopicIgnoringErrors topic acc =
      case fromDbTopic topic of
        Left _ -> acc
        Right t -> t : acc

deleteTopic :: FirstAppDB -> Topic -> IO (Either Error ())
deleteTopic db topic = do
  Sql.execute (dbConn db) sql [getTopic topic]
  return $ pure ()
  where
    sql = "DELETE FROM comments WHERE topic = ?"
