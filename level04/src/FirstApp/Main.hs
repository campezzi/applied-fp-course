{-# LANGUAGE OverloadedStrings #-}

module FirstApp.Main
  ( runApp
  , prepareAppReqs
  , app
  , loadConfig
  ) where

import Control.Applicative (liftA2)
import Control.Monad (join)

import Network.Wai
       (Application, Request, Response, pathInfo, requestMethod,
        responseLBS, strictRequestBody)
import Network.Wai.Handler.Warp (run)

import Network.HTTP.Types
       (Status, hContentType, status200, status400, status404, status500)

import qualified Data.ByteString.Lazy.Char8 as LBS

import Data.Either (Either(Left, Right), either)

import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as A

import Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import Debug.Trace (trace)

import FirstApp.Conf (Conf, dbFilePath, serverPort)
import qualified FirstApp.DB as DB
import FirstApp.Types
       (ContentType(JSON, PlainText),
        Error(DatabaseError, EmptyCommentText, EmptyTopic, UnknownRoute),
        RqType(AddRq, ListRq, ViewRq), mkCommentText, mkTopic,
        renderContentType)

-- Our start-up is becoming more complicated and could fail in new and
-- interesting ways. But we also want to be able to capture these errors in a
-- single type so that we can deal with the entire start-up process as a whole.
data StartUpError
  = ConfigErr
  | DbInitErr SQLiteResponse
  deriving (Show)

runApp :: IO ()
runApp = do
  appReqs <- prepareAppReqs
  case appReqs of
    Left e -> putStrLn $ "Error initializing server: " ++ show e
    Right (conf, db) -> do
      putStrLn $ "Started server on port " ++ show port
      run port (app db)
      where port = serverPort conf

-- We need to complete the following steps to prepare our app requirements:
--
-- 1) Load the configuration.
-- 2) Attempt to initialise the database.
-- 3) Combine the results into a tuple
--
-- The filename for our application config is: "appconfig.json"
--
prepareAppReqs :: IO (Either StartUpError (Conf, DB.FirstAppDB))
prepareAppReqs = do
  config <- loadConfig
  case config of
    Left e -> pure $ Left e
    Right conf -> do
      result <- DB.initDB $ dbFilePath conf
      case result of
        Left e -> pure $ Left $ DbInitErr e
        Right db -> pure $ Right (conf, db)

loadConfig :: IO (Either StartUpError Conf)
loadConfig = do
  config <- A.decode <$> LBS.readFile "appconfig.json"
  pure . maybe (Left ConfigErr) Right $ config

-- | Some helper functions to make our lives a little more DRY.
mkResponse :: Status -> ContentType -> LBS.ByteString -> Response
mkResponse sts ct = responseLBS sts [(hContentType, renderContentType ct)]

resp200 :: ContentType -> LBS.ByteString -> Response
resp200 = mkResponse status200

resp404 :: ContentType -> LBS.ByteString -> Response
resp404 = mkResponse status404

resp400 :: ContentType -> LBS.ByteString -> Response
resp400 = mkResponse status400

-- Some new helpers for different statuses and content types
resp500 :: ContentType -> LBS.ByteString -> Response
resp500 = mkResponse status500

resp200Json :: ToJSON a => a -> Response
resp200Json = mkResponse status200 JSON . A.encode

-- |
app ::
     DB.FirstAppDB -- ^ Add the Database record to our app so we can use it
  -> Application
app db rq cb = do
  rq' <- mkRequest rq
  resp <- handleRespErr <$> handleRErr rq'
  cb resp
  where
    handleRespErr :: Either Error Response -> Response
    handleRespErr = either mkErrorResponse id
    -- We want to pass the Database through to the handleRequest so it's
    -- available to all of our handlers.
    handleRErr :: Either Error RqType -> IO (Either Error Response)
    handleRErr = either (pure . Left) (handleRequest db)

handleRequest :: DB.FirstAppDB -> RqType -> IO (Either Error Response)
handleRequest _db (AddRq _ _) =
  (resp200 PlainText "Success" <$) <$> error "AddRq handler not implemented"
handleRequest _db (ViewRq _) = error "ViewRq handler not implemented"
handleRequest _db ListRq = error "ListRq handler not implemented"

mkRequest :: Request -> IO (Either Error RqType)
mkRequest rq =
  case (pathInfo rq, requestMethod rq) of
    ([t, "add"], "POST") -> mkAddRequest t <$> strictRequestBody rq
    ([t, "view"], "GET") -> pure (mkViewRequest t)
    (["list"], "GET") -> pure mkListRequest
    _ -> pure (Left UnknownRoute)

mkAddRequest :: Text -> LBS.ByteString -> Either Error RqType
mkAddRequest ti c =
  AddRq <$> mkTopic ti <*> (mkCommentText . decodeUtf8 . LBS.toStrict) c

mkViewRequest :: Text -> Either Error RqType
mkViewRequest = fmap ViewRq . mkTopic

mkListRequest :: Either Error RqType
mkListRequest = Right ListRq

mkErrorResponse :: Error -> Response
mkErrorResponse UnknownRoute = resp404 PlainText "Unknown Route"
mkErrorResponse EmptyCommentText = resp400 PlainText "Empty Comment"
mkErrorResponse EmptyTopic = resp400 PlainText "Empty Topic"
mkErrorResponse DatabaseError = resp500 PlainText "Database Error"
