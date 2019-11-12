{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass    #-}

module Main where

import Data.Aeson
import Data.Text
import qualified Data.Text.Lazy as T
import GHC.Generics
import Data.HashMap.Strict 
import qualified Data.Text.Lazy.Encoding as T
import Web.Scotty
import System.Process
import Control.Monad.IO.Class (liftIO)

data EvalRequest = 
    EvalRequest { 
        _script :: Text, 
        _data :: HashMap Text Value
      } deriving (Show, Generic, ToJSON)

instance FromJSON EvalRequest 
    where parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = Prelude.drop 1 }


infoResponse = "{\"description\": \"\", \"creation_time\": \"0\", \"state_path\": \"\", \"server_version\": \"0.8.7\", \"name\": \"haskell\", \"versions\": {\"v1\": {\"features\": {}}}}"

textAsJson t = setHeader "Content-Type" "application/json" >> text t

main = scotty 3000 $ do
    get "/info" $ 
        textAsJson infoResponse
    post "/evaluate" $ do
        jsonData <- body 
        cmdRes <- liftIO $ executeCommand jsonData
        textAsJson $ T.pack cmdRes