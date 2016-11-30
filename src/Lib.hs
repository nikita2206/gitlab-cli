{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveDataTypeable #-}

module Lib
    ( app
    , GlArgs(GlCreateMr)
    , GlConfig(GlConfig)
    ) where

import Network.Wreq
import Control.Lens
import Text.Printf
import Data.Aeson
import Data.Text as T
import GHC.Generics
import Data.Map as M

data GlConfig = GlConfig {
    privateToken :: String
  , url :: String
  } deriving (Generic, Show)
instance ToJSON GlConfig where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON GlConfig

data GlArgs =
  GlCreateMr { targetBranch :: String, title :: Maybe String, sourceBranch :: Maybe String }
  deriving (Eq, Show, Data, Typeable)

app :: GlConfig -> GlArgs -> IO ()
app config (GlCreateMr targetBranch title sourceBranch) = do
  let params = defaults & (param "private_token" .~ [T.pack $ privateToken config])
  r <- postWith params (url config) [
      "source_branch" := sourceBranch
    , "target_branch" := targetBranch
    , "title" := title
    ]
  print (r ^? responseStatus . statusCode)
