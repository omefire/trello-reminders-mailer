{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module DB (getReminders, getRemindersWhoseNotificationTimeAlreadyPassed) where

import Types
import Opaleye
import Data.Time
import Opaleye.Trans
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import qualified Database.PostgreSQL.Simple as PSQL
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Arrow (returnA)


data ReminderP i n d dt p = ReminderP
  { remID :: i
  , remName :: n
  , remDescription :: d
  , remDateTime :: dt
  , remProcessed :: p
  } deriving (Show, Eq)

data ReminderResult i n d dt e = ReminderResult
  { rrID :: i
  , rrName :: n
  , rrDesc :: d
  , rrDt :: dt
  , rrEmails :: e
  } deriving (Show, Eq)

data ReminderEmailP a b = ReminderEmailP
  { reReminderID :: a
  , reEmailID :: b
  }

data EmailP a b = EmailP
  { emID :: a
  , emEmail :: b
  }

type WriteReminder = ReminderP (Maybe (Column PGInt4)) (Column PGText) (Column PGText) (Column PGTimestamp) (Column PGBool)
type ReadReminder = ReminderP (Column PGInt4) (Column PGText) (Column PGText) (Column PGTimestamp) (Column PGBool)

type WriteReminderResult = ReminderResult (Column PGInt4) (Column PGText) (Column PGText) (Column PGTimestamptz) [(Column PGText)]
type ReadReminderResult = ReminderResult (Column PGInt4) (Column PGText) (Column PGText) (Column PGTimestamptz) [(Column PGText)]

type WriteReminderEmail = ReminderEmailP (Column PGInt4) (Column PGInt4)
type ReadReminderEmail = ReminderEmailP (Column PGInt4) (Column PGInt4)

type WriteEmail = EmailP (Maybe (Column PGInt4)) (Column PGText)
type ReadEmail = EmailP (Column PGInt4) (Column PGText)

makeAdaptorAndInstance "pReminder" ''ReminderP
makeAdaptorAndInstance "pReminderEmail" ''ReminderEmailP
makeAdaptorAndInstance "pEmail" ''EmailP

reminderTable :: Table (WriteReminder) (ReadReminder)
reminderTable = Table "Reminders" $ pReminder ReminderP
  { remID = optional "ID"
  , remName = required "Name"
  , remDescription = required "Description"
  , remDateTime = required "ReminderDateTime"
  , remProcessed = required "Processed"
  }

reminderEmailTable :: Table (WriteReminderEmail) (ReadReminderEmail)
reminderEmailTable = Table "Reminders_Emails" $ pReminderEmail ReminderEmailP
  { reReminderID = required "ReminderID"
  , reEmailID = required "EmailID"
  }

emailTable :: Table (WriteEmail) (ReadEmail)
emailTable = Table "Emails" $ pEmail EmailP
  { emID = optional "ID"
  , emEmail = required "Email"
  }


getReminderEmails :: Int -> Transaction [ String ]
getReminderEmails reminderId = query $ reminderEmailsQuery reminderId
  where
    reminderEmailsQuery :: Int -> Query ( (Column PGText) )
    reminderEmailsQuery reminderId = proc() -> do
        rems <- selectTable reminderTable -< ()
        restrict -< (remID rems) .== (pgInt4 reminderId)

        remEmail <- selectTable reminderEmailTable -< ()
        restrict -< (reReminderID remEmail) .== (remID rems)

        email <- selectTable emailTable -< ()
        restrict -< (reEmailID remEmail) .== (emID email)

        returnA -< (emEmail email)

-- ============  TOP LEVEL FUNCTIONS ============= --

getRemindersWhoseNotificationTimeAlreadyPassed :: PSQL.Connection -> LocalTime -> IO [Reminder]
getRemindersWhoseNotificationTimeAlreadyPassed conn notificationTime = do
  reminders <- runOpaleyeT conn $ transaction $ do
    rems <- selectReminders
    (flip mapM) rems $ \(ReminderP rId rName rDesc rDT rProcessed) -> do
      rEmails <- getReminderEmails rId
      return (rId, rName, rDesc, rDT, rEmails, rProcessed)
  return $ (flip map) reminders $ \( (rrID, rrName, rrDesc, rrDt, rrEmails, rrProcessed) ) -> Reminder { reminderID = rrID,
                                                                                                         reminderName = rrName,
                                                                                                         reminderDescription = rrDesc,
                                                                                                         reminderDateTime = rrDt,
                                                                                                         reminderEmails = rrEmails,
                                                                                                         reminderProcessed = rrProcessed
                                                                                                       }
    where
      selectReminders :: Transaction [ ReminderP Int String String LocalTime Bool ]
      selectReminders = query $ remindersQuery

      remindersQuery :: Query ReadReminder
      remindersQuery = proc() -> do
        rems <- selectTable reminderTable -< ()
        restrict -< (remDateTime rems) .<= (pgLocalTime notificationTime)
        restrict -< (remProcessed rems) ./= (pgBool True)
        returnA -< (rems)

getReminders :: PSQL.Connection -> LocalTime -> LocalTime -> Bool -> IO [Reminder]
getReminders conn beginDate endDate processed = do
  reminders <- runOpaleyeT conn $ transaction $ do
    rems <- selectReminders
    (flip mapM) rems $ \(ReminderP rId rName rDesc rDT rProcessed) -> do
      rEmails <- getReminderEmails rId
      return (rId, rName, rDesc, rDT, rEmails, rProcessed)
  return $ (flip map) reminders $ \( (rrID, rrName, rrDesc, rrDt, rrEmails, rrProcessed) ) -> Reminder { reminderID = rrID,
                                                                                                         reminderName = rrName,
                                                                                                         reminderDescription = rrDesc,
                                                                                                         reminderDateTime = rrDt,
                                                                                                         reminderEmails = rrEmails,
                                                                                                         reminderProcessed = rrProcessed
                                                                                                       }
    where
      selectReminders :: Transaction [ ReminderP Int String String LocalTime Bool ]
      selectReminders = query $ remindersQuery

      remindersQuery :: Query ReadReminder
      remindersQuery = proc() -> do
        rems <- selectTable reminderTable -< ()
        restrict -< (remDateTime rems) .>= (pgLocalTime beginDate)
        restrict -< (remDateTime rems) .<= (pgLocalTime endDate)
        restrict -< (remProcessed rems) ./= (pgBool processed)
        returnA -< (rems)
