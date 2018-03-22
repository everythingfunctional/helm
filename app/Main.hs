{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import           Data.Aeson   (ToJSON)
import           GHC.Generics (Generic)
import           Servant      ((:<|>), (:>), Capture, Get, JSON)

data Task = Task
    { taskId      :: Int
    , description :: String
    } deriving (Eq, Show, Generic)

instance ToJSON Task

type HelmAPI =
        Get '[JSON] [Task]
    :<|> Capture "taskId" Int :> Get '[JSON] Task

main = putStrLn "Hello"
