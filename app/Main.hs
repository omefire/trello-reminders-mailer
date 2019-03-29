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
import Data.Foldable (forM_)
import Control.Concurrent.MVar
import Data.List (take)

data MailerException = DBConnectionIssue !String
  deriving (Show)

instance Exception MailerException

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

atomicPutStrLn :: String -> IO ()
atomicPutStrLn str = do
  lock <- newMVar ()
  takeMVar lock >> putStrLn str >> putMVar lock ()

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
        let senderEmail = "Trello Reminders <info@trelloreminders.com>"

        _ <- forkIO $ do
          let beginDate = utcToLocalTime utc currentTime -- now()
          let endDate = utcToLocalTime utc (addUTCTime (threeMins) currentTime) -- now() + 3mins

          reminders <- do
            rems1 <- (getReminders conn beginDate endDate False)
            rems2 <- (getRemindersWhoseNotificationTimeAlreadyPassed conn beginDate)
            return (rems1 <> rems2)

          forM_ reminders $ \reminder -> do
            forM_ (reminderEmails reminder) $ \reminderEmail -> do
              let from = Email.From senderEmail
              let to = Email.To $ pack reminderEmail
              forkIO $ catchAny
                (do
                    -- Test: What if email is successfully sent? => email received + processed column = true (DONE)
                    -- Test: What if email sending fails? => email not sent + processed column = false + exception caught (DONE)
                    -- Test: What if email sending succeeds, but processed column does not succeed in being marked true? => The user will have to live with multiple emails received (DONE)
                    _ <- Email.sendEmail from to reminder
                    -- atomicPutStrLn "Email sent !!!!!!!!!!!!!!!"
                    return ()
                )
                (\ex -> do
                    atomicPutStrLn $ "An exception occured: " <> show ex
                )
            -- TODO: In the future, instead of marking a reminder as processed because we have attempted to send emails to its recipients,
            --    ... we should instead, flag it for human review in case any of the emails sent failed
            markReminderAsProcessed conn (reminderID reminder)

        -- TODO: How to handle exceptions raised by another thread? => Not necessary (DONE)
        -- TODO: Log stuff into a log file, not on the console
        -- TODO: If sending an email to a recipient fails, it should not impact another one
        -- TODO: Mark emails as processed once completed (DONE)
        -- TODO: What if email sending fails? Bad AWS SES credentials? Wrong/Inexistent email address? Other reasons? => Exception logged & process continues (DONE)
        -- TODO: What if this program crashes? Should we reload all reminders that should have been processed in the past, but weren't? => They'll be sent upon program execution (DONE)
        -- TODO: In case of program crash or restarting, should we process past reminders that should have been processed but weren't? => YES (DONE)
        -- TODO: How can we make the query not scan the whole Reminders table? => TODO
        -- TODO: What if AWS SES is down? Let the users know we're currently unstable? (Very rare occasion probably) => No! (DONE)
        -- TODO: Test: while this program is already running, make sure to add a new reminder to the db that should execute in the next 2 mins and see if it gets processed correctly (DONE)
        -- TODO: Test: while this program is already running, make sure to add a new reminder to the db that should execute in the next 5 mins and see if it gets processed correctly (DONE)
        -- TODO: Test: while this program is already running, make sure to add a new reminder to the db that should execute in the next 3 mins and see if it gets processed correctly (DONE)
        -- TODO: Brainstorm other what/if scenarios (DONE)
        -- TODO: Send off emails in separate threads (DONE)
        -- TODO: A reminder that should notify multiple email addresses (DONE)
        atomicPutStrLn "ONE LOOP DONE!"
        threadDelay 180000000 -- Pause execution for at least 3 mins
