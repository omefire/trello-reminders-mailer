{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module ConnectionInfo (getConnectionInfo, Info(..)) where

import System.IO (readFile, stderr, hPutStrLn)
import GHC.Generics
import Data.Aeson
import Data.Text
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import Control.Exception (catch, IOException)

data Info = Info { host :: String
                 , port :: Int
                 , database :: String
                 , user :: String
                 , password :: String
                 } deriving (Show, Generic)

instance FromJSON Info
instance ToJSON Info

jsonFile :: FilePath
jsonFile = "credentials.json"

getJSON :: IO (Either String Info)
getJSON = do
  json <- B.readFile jsonFile
  let mJson = decode json
  case mJson of
    Nothing -> return $ Left "An error occured while parsing the JSON data"
    Just json -> return $ Right json

-- TODO: Move this into a separate package so that it can be used by this project as well as trello-reminders-server project
getConnectionInfo :: IO (Either String Info)
getConnectionInfo = do
  catch getJSON (\e -> do
                    let err = show (e :: IOException)
                    let msg = "Couldn't open credentials.json because of : " ++ err
                    hPutStrLn stderr msg
                    return $ Left msg)
