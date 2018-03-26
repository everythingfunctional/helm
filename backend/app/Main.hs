{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Main where

import           Data.Aeson               (ToJSON)
import           Data.List                (find)
import           GHC.Generics             (Generic)
import           Network.Wai.Handler.Warp (run)
import           Servant                  ((:<|>) (..), (:>), Application,
                                           Capture, Get, Handler, JSON,
                                           Proxy (..), ServantErr, Server,
                                           err404, errBody, serve, throwError)

data Task = Task
    { taskId      :: Int
    , description :: String
    } deriving (Eq, Show, Generic)

instance ToJSON Task

type HelmAPI =
        Get '[JSON] [Task]
        :<|> Capture "taskId" Int :> Get '[JSON] Task

tasks :: [Task]
tasks =
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
    a = find ((== idParam) . taskId) tasks

helmServer :: Server HelmAPI
helmServer = return tasks :<|> taskById

type API = "tasks" :> HelmAPI

api :: Proxy API
api = Proxy

app :: Application
app = serve api helmServer

main :: IO ()
main = run 3000 app
