{-# LANGUAGE DeriveGeneric #-}
module Models
    (Task (Task, taskId)
    ) where

import           Data.Aeson   (ToJSON)
import           GHC.Generics (Generic)


data Task = Task
    { taskId      :: Int
    , description :: String
    } deriving (Eq, Show, Generic)

instance ToJSON Task
