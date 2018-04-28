{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module App (runApp) where

import           Api                         (ApiWithAssets, TaskApi)
import           Data.List                   (find)
import           Models                      (Task (Task, taskId))
import           Network.Wai                 (Middleware)
import           Network.Wai.Handler.Warp    (run)
import           Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors,
                                              simpleHeaders)
import           Servant                     ((:<|>) ((:<|>)), (:>),
                                              Application, Handler,
                                              Proxy (Proxy), Raw, Server,
                                              err404, errBody, serve,
                                              serveDirectoryFileServer,
                                              throwError)

runApp :: IO ()
runApp = run 3000 $ helmCors app

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

app :: Application
app = serve api (taskServer :<|> rawServer)


api :: Proxy ApiWithAssets
api = Proxy


taskServer :: Server TaskApi
taskServer = return defaultTasks :<|> taskById

defaultTasks :: [Task]
defaultTasks =
    [ Task 1 "Say hello"
    , Task 2 "Say goodbye"
    , Task 3 "Make stuff up"
    , Task 4 "Foobar"
    ]

taskById :: Int -> Handler Task
taskById idParam =
  case maybeTask of
    Nothing   -> throwError err404 {errBody = "No task with given id exists"}
    Just task -> return task
  where
    maybeTask = find ((== idParam) . taskId) defaultTasks


rawServer :: Server Raw
rawServer = serveDirectoryFileServer "frontend/dist"
