{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Main where

import           Control.Monad.Trans.Either (EitherT, left)
import           Data.Aeson                 (ToJSON)
import           Data.List                  (find)
import           GHC.Generics               (Generic)
import           Servant                    ((:<|>), (:>), Capture, Get, JSON,
                                             ServantErr, err404, errBody)

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

taskById :: Int -> EitherT ServantErr IO Task
taskById idParam =
  case a of
    Nothing -> left (err404 {errBody = "No artist with given id exists"})
    Just b  -> return b
  where
    a = find ((== idParam) . taskId) tasks

main = putStrLn "Hello"
