{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveDataTypeable #-}

module Main where

import Lib
import System.Console.CmdArgs
import System.Console.CmdArgs.Explicit (processArgs)
import System.Directory (getHomeDirectory)
import Control.Exception
import Control.Arrow (left)
import Data.Map as M
import Data.ByteString.Lazy as B
import Data.Aeson

data CommonConfig = CommonConfig String GlArgs deriving (Data, Typeable, Show, Eq)

mode = cmdArgsMode $ modes [
    GlCreateMr {
        targetBranch = def &= typ "TARGET_BRANCH" &= argPos 1
      , title = def &= typ "TITLE"
      , sourceBranch = def &= typ "BRANCH" &= name "source-branch"
    } &= help "Create a new Merge Request" &= explicit &= name "new"
  ] &= help "Manipulate or view Merge Requests in gitlab" &= program "gl"

main = do
  args <- cmdArgsRun mode
  configAll <- loadConfig
  let args = CommonConfig "sf" (GlCreateMr "dev" (Just "test-mr") (Just "PHP-1234"))

  runWithArgs configAll args

runWithArgs :: (M.Map String GlConfig) -> CommonConfig -> IO ()
runWithArgs configAll (CommonConfig project args) = do
  case M.lookup project configAll of
    Just config -> app config args
    Nothing -> print $ "Couldn't find configuration for project " ++ project ++ "\n"

data ConfigReadingException =
  ConfigNotFoundException String |
  ConfigDecodingException String
instance Exception ConfigReadingException
instance Show ConfigReadingException where
  show (ConfigNotFoundException filename) =
    "Couldn't read configuration from " ++ filename ++ "\nFile doesn't exist.\n"
  show (ConfigDecodingException msg) =
    "Couldn't decode configuration: " ++ msg ++ "\n"

loadConfig :: IO (M.Map String GlConfig)
loadConfig = do
  result <- loadConfigEither
  case result of
    (Left e) -> throwIO e
    (Right v) -> return v

loadConfigEither :: IO (Either ConfigReadingException (M.Map String GlConfig))
loadConfigEither = do
  homeDir <- getHomeDirectory
  let filename = homeDir ++ "/.gl-config"
  jsonConfig <- B.readFile $ filename
  return $ left (\msg -> ConfigDecodingException msg) (eitherDecode jsonConfig)
