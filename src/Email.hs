{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Email (Email.sendEmail, From(..), To(..)) where

import Types

import Network.AWS
import Network.AWS.SES

import Control.Lens
import System.IO

import Data.Text

import System.Environment

import Control.Monad.Trans.AWS as CMTA
import Control.Exception.Safe (throwString)
import Control.Exception(throw)
import qualified Data.ByteString.Char8 as BC

import AWSConfig

newtype From = From Text
newtype To = To Text

sendEmail :: From -> To -> Reminder -> IO ()
sendEmail from to reminder  = do
  _awsConfig <- getAWSConfig
  case _awsConfig of
    Left err -> throwString err
    Right AWSConfig { accessKey, secretKey } -> do
      env' <- newEnv (FromKeys (AccessKey $ BC.pack accessKey) (SecretKey $ BC.pack secretKey))
      let env = env' & envRegion .~ CMTA.Oregon -- 'us-west-2'

      -- TODO: Write to a log file
      logger <- newLogger Debug stdout
      _ <- runResourceT . runAWS (env & envLogger .~ logger) $ (Network.AWS.send $ createEmail from to reminder)
      return ()

createEmail :: From -> To -> Reminder -> SendEmail
createEmail (From sender) (To recipient) reminder = Network.AWS.SES.sendEmail sender dest msg
  where
    dest = destination & dToAddresses .~ [recipient]
    msg = message content' body'
    content' = content "" & cData .~ ( (pack "[Reminder] ") <> (pack $ reminderName reminder))
    body' = body & bHTML .~ (Just (content $
                                      "Hi " <> recipient <> ", "
                                   <> "<br>"
                                   <> "<br>"
                                   <> "<div>"
                                   <> "This email is from Trello Reminders, to remind you about the following:"
                                   <> "<br>"
                                   <> "<br>"
                                   <> "<b>Name of reminder: </b> &nbsp;" <> (pack $ reminderName reminder)
                                   <> "<br>"
                                   <> "<br>"
                                   <> "<b>Description of reminder: </b> &nbsp;" <> (pack $ reminderDescription reminder)
                                   <> "<br>"
                                   <> "<br>"
                                   <> "<div style=\"font-family:arial,sans-serif\">-----</div>"
                                   <> "<i>You can contact us at info@trelloreminders.com</i>"
                                   <> "<div style=\"font-family:arial,sans-serif\">-----</div>"
                                   <> "<br>"
                                   <> "Trello Reminders"
                                   <> "<br>"
                                   <> "www.trelloreminders.com"
                                  ))
