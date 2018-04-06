{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module Types
    ( ApiWithAssets
    , Task (Task, taskId)
    , TaskApi
    ) where

import           Data.Aeson   (ToJSON)
import           GHC.Generics (Generic)
import           Servant      ((:<|>), (:>), Capture, Get, JSON, Raw)

type ApiWithAssets = HelmApi :<|> Raw

type HelmApi = "api" :> TaskApi

type TaskApi =
        "tasks" :> (Get '[JSON] [Task]
        :<|> Capture "taskId" Int :> Get '[JSON] Task)

data Task = Task
    { taskId      :: Int
    , description :: String
    } deriving (Eq, Show, Generic)

instance ToJSON Task
