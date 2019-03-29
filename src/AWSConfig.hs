{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module AWSConfig (getAWSConfig, AWSConfig(..)) where

import System.IO (readFile, stderr, hPutStrLn)
import GHC.Generics
import Data.Aeson
import Data.Text
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import Control.Exception (catch, IOException)

data AWSConfig = AWSConfig { accessKey :: String
                           , secretKey :: String
                           } deriving (Show, Generic)

instance FromJSON AWSConfig
instance ToJSON AWSConfig

jsonFile :: FilePath
jsonFile = "awsConfig.json"

getJSON :: IO (Either String AWSConfig)
getJSON = do
  json <- B.readFile jsonFile
  let mJson = decode json
  case mJson of
    Nothing -> return $ Left "An error occured while parsing the JSON data"
    Just json -> return $ Right json

getAWSConfig :: IO (Either String AWSConfig)
getAWSConfig = do
  catch getJSON (\e -> do
                    let err = show (e :: IOException)
                    let msg = "Couldn't open awsConfig.json because of : " ++ err
                    hPutStrLn stderr msg
                    return $ Left msg)
