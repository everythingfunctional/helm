{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Main where

import           Data.Aeson                  (ToJSON)
import           Data.List                   (find)
import           GHC.Generics                (Generic)
import           Network.Wai                 (Middleware)
import           Network.Wai.Handler.Warp    (run)
import           Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors,
                                              simpleHeaders)
import           Servant                     ((:<|>) (..), (:>), Application,
                                              Capture, Get, Handler, JSON,
                                              Proxy (..), ServantErr, Server,
                                              err404, errBody, serve,
                                              throwError)

data Task = Task
    { taskId      :: Int
    , description :: String
    } deriving (Eq, Show, Generic)

instance ToJSON Task

type TaskAPI =
        Get '[JSON] [Task]
        :<|> Capture "taskId" Int :> Get '[JSON] Task

defaultTasks :: [Task]
defaultTasks =
    [ Task 1 "Say hello"
    , Task 2 "Say goodbye"
    , Task 3 "Make stuff up"
    , Task 4 "Foobar"
    ]

taskById :: Int -> Handler Task
taskById idParam =
  case a of
    Nothing -> throwError err404 {errBody = "No task with given id exists"}
    Just b  -> return b
  where
    a = find ((== idParam) . taskId) defaultTasks

taskServer :: Server TaskAPI
taskServer = return defaultTasks :<|> taskById

type HelmAPI = "tasks" :> TaskAPI

api :: Proxy HelmAPI
api = Proxy

app :: Application
app = serve api taskServer

helmCors :: Middleware
helmCors = cors $ const (Just helmResourcePolicy)

helmResourcePolicy :: CorsResourcePolicy
helmResourcePolicy = CorsResourcePolicy
        { corsOrigins = Nothing -- gives you /*
        , corsMethods = ["GET", "POST", "PUT", "DELETE", "HEAD", "OPTION"]
        , corsRequestHeaders = simpleHeaders -- adds "Content-Type" to defaults
        , corsExposedHeaders = Nothing
        , corsMaxAge = Nothing
        , corsVaryOrigin = False
        , corsRequireOrigin = False
        , corsIgnoreFailures = False
        }

main :: IO ()
main = run 3000 $ helmCors app
