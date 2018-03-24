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

tasks :: [Task]
tasks =
    [ Task 1 "Say hello"
    , Task 2 "Say goodbye"
    , Task 3 "Make stuff up"
    , Task 4 "Foobar"
    ]

main = putStrLn "Hello"
