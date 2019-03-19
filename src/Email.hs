{-# LANGUAGE OverloadedStrings #-}

module Email (Email.sendEmail, From(..), To(..)) where

import Types

import Network.AWS
import Network.AWS.SES

import Control.Lens
import System.IO

import Data.Text

import System.Environment

import Control.Monad.Trans.AWS as CMTA

newtype From = From Text
newtype To = To Text

-- TODO: Exception/Error Handling
sendEmail :: From -> [To] -> Reminder -> IO ()
sendEmail sender to's reminder  = do
  -- TODO: Get access key, secret key, region, source email and 'from' email from a config file
  -- awsConfig <- getAWSConfig
  let accessKey = AccessKey ""
  let secretKey = SecretKey ""
  putStrLn =<< getEnv "AWS_REGION"
  env' <- newEnv (FromKeys accessKey secretKey) -- TODO: Set AWS_REGION env var to 'us-west-2'
  let env = env' & envRegion .~ CMTA.Oregon
  logger <- newLogger Debug stdout
  _ <- runResourceT . runAWS (env & envLogger .~ logger) $ (Network.AWS.send $ createEmail sender (to's !! 0) reminder) -- TODO: Fix to's
  return ()

createEmail :: From -> To -> Reminder -> SendEmail
createEmail (From sender) (To recipient) reminder = Network.AWS.SES.sendEmail sender dest msg
  where
    dest = destination & dToAddresses .~ [recipient]
    msg = message content' body'
    content' = content "" & cData .~ ( (pack "[Reminder] ") <> (pack $ reminderName reminder))
    body' = body & bHTML .~ (Just (content $
                                      "Hi " <> recipient <> ", "
                                   <> "<div>"
                                   <> "This email is from Trello Reminders, to remind you about the following:"
                                   <> "<br>"
                                   <> "<br>"
                                   <> "Name of reminder: &nbsp;" <> (pack $ reminderName reminder)
                                   <> "<br>"
                                   <> "Description of reminder: " <> (pack $ reminderDescription reminder)
                                   <> "<br>"
                                   <> "<br>"
                                   <> "<div style=\"font-family:arial,sans-serif\">-----</div>"
                                   <> "<i>You can contact us at info@trelloreminders.com</i>"
                                   <> "<div style=\"font-family:arial,sans-serif\">-----</div>"
                                   <> "Trello Reminders"
                                   <> "www.trelloreminders.com"
                                  ))
