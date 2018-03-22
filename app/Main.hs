{-# LANGUAGE DeriveGeneric #-}
module Main where

import           GHC.Generics (Generic)

data Task = Task
    { taskId      :: Int
    , description :: String
    } deriving (Eq, Show, Generic)

main = putStrLn "Hello"
