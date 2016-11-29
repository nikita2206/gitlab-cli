{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import System.Console.ArgParser
import Network.Wreq
import Control.Lens
import Text.Printf
import Data.Aeson
import Data.Text as T
import GHC.Generics
import Data.Map as M
import Control.Exception
import Data.ByteString.Lazy as B
import System.Directory (getHomeDirectory)

data GlArgs =
  GlCreateMr String String String String
  deriving (Eq, Show)

glArgParser :: IO (CmdLnInterface GlArgs)
glArgParser = mkSubParser
  [
    ("mr", mkDefaultApp (GlCreateMr `parsedBy`
      reqPos "target-branch" `andBy`
      reqPos "title" `andBy`
      optFlag "" "source-branch" `andBy`
      optFlag "_" "project") "mr")
  ]

main = do
  interface <- glArgParser
  runApp interface app

app :: GlArgs -> IO ()
app (GlCreateMr targetBranch title sourceBranch project) = do
  configAll <- loadConfig
  case M.lookup project configAll of
    Nothing -> throwIO ConfigNotFoundException
    (Just config) -> do
      let params = defaults & (param "private_token" .~ [T.pack $ privateToken config])
      r <- postWith params (url config) [
          "source_branch" := sourceBranch
        , "target_branch" := targetBranch
        , "title" := title
        ]
      print (r ^? responseStatus . statusCode)

data GlConfig = GlConfig {
    privateToken :: String
  , url :: String
} deriving (Generic, Show)

instance ToJSON GlConfig where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON GlConfig

data ConfigNotFoundException = ConfigNotFoundException deriving (Show)
instance Exception ConfigNotFoundException

loadConfig :: IO (M.Map String GlConfig)
loadConfig = do
  homeDir <- getHomeDirectory
  jsonConfig <- B.readFile $ homeDir ++ "/.gl-config"
  case decode jsonConfig :: Maybe (M.Map String GlConfig) of
    (Just config) -> return config
    Nothing -> throwIO ConfigNotFoundException
