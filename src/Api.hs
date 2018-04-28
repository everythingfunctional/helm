{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Api
    ( ApiWithAssets
    , TaskApi
    ) where

import           Data.Aeson (ToJSON)
import           Models     (Task)
import           Servant    ((:<|>), (:>), Capture, Get, JSON, Raw)

type ApiWithAssets = HelmApi :<|> Raw

type HelmApi = "api" :> TaskApi

type TaskApi =
        "tasks" :> (Get '[JSON] [Task]
        :<|> Capture "taskId" Int :> Get '[JSON] Task)
