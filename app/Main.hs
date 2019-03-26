{-# LANGUAGE OverloadedStrings #-}

module Main where

import DB
import Types
import Data.Time
import Control.Monad (forever)
import Control.Concurrent (threadDelay, forkIO)
import ConnectionInfo as CI
import Control.Exception
import Control.Monad.IO.Class (liftIO)
import Database.PostgreSQL.Simple
import Email as Email
import Data.Text as T
import System.IO
import Data.Time.LocalTime

data MailerException = DBConnectionIssue !String
  deriving (Show)

instance Exception MailerException

main :: IO ()
main = do
  eConnInfo <- liftIO $ CI.getConnectionInfo
  case eConnInfo of
    Left err -> throw $ DBConnectionIssue err
    Right connInfo -> do
      conn <- liftIO $ connect ConnectInfo {connectHost = host connInfo
                                           ,connectPort = (fromIntegral $ port connInfo)
                                           ,connectDatabase = database connInfo
                                           ,connectPassword = password connInfo
                                           ,connectUser = user connInfo
                                           }
      hSetBuffering stdout NoBuffering
      forever $ do
        currentTime <- getCurrentTime
        let threeMins = 3 * 60
        let sender = "Trello Reminders <info@trelloreminders.com>"
        forkIO $ do
          let beginDate' =  currentTime -- addUTCTime (- threeMins) currentTime -- now() - 3 mins
          let endDate' = addUTCTime (threeMins) currentTime  -- now() + 3 mins
          let beginDate = utcToLocalTime utc beginDate'
          let endDate = utcToLocalTime utc endDate'
          reminders <- getReminders conn beginDate endDate
          -- putStrLn $ show $ Prelude.length reminders
          (flip mapM_) reminders $ \reminder -> do
            forkIO $ Email.sendEmail (Email.From sender) (Prelude.map (\re -> Email.To (T.pack $ re)) (reminderEmails reminder)) reminder

        -- mapM_ Main.sendEmail reminders
        -- TODO: Mark emails as processed once completed
        -- TODO: What if email sending fails?
        -- TODO: What if this program crashes? Should we reload all reminders that should have been processed in the past, but weren't?
        -- TODO: In case of program crash or restarting, should we process past reminders that should have been processed but weren't?
        -- TODO: How can we make the query not scan the whole Reminders table?
        -- TODO: What if AWS SES is down? Let the users know we're currently unstable? (Very rare occasion probably)
        -- TODO: Test: while this program is already running, make sure to add a new reminder to the db that should execute in the next 2 mins and see if it gets processed correctly
        -- TODO: Test: while this program is already running, make sure to add a new reminder to the db that should execute in the next 5 mins and see if it gets processed correctly
        -- TODO: Test: while this program is already running, make sure to add a new reminder to the db that should execute in the next 3 mins and see if it gets processed correctly
        -- TODO: Brainstorm other what/if scenarios
        -- TODO: Send off emails in separate threads
        putStrLn "ONE LOOP DONE!"
        threadDelay 30000

-- TODO: After email is sent successfully, mark it as processed in the database
sendEmail :: Reminder -> IO ()
sendEmail _rem = putStrLn $ "Reminder: " ++ (reminderName _rem)
