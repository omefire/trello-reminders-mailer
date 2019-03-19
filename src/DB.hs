{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module DB (getReminders) where

import Types
import Opaleye
import Data.Time
import Opaleye.Trans
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import qualified Database.PostgreSQL.Simple as PSQL
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Arrow (returnA)


data ReminderP i n d dt = ReminderP
  { remID :: i
  , remName :: n
  , remDescription :: d
  , remDateTime :: dt
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

type WriteReminder = ReminderP (Maybe (Column PGInt4)) (Column PGText) (Column PGText) (Column PGTimestamptz)
type ReadReminder = ReminderP (Column PGInt4) (Column PGText) (Column PGText) (Column PGTimestamptz)

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

-- ============  TOP LEVEL FUNCTIONS ============= --

getReminders :: PSQL.Connection -> UTCTime -> UTCTime -> IO [Reminder]
getReminders conn beginDate endDate = do
  reminders <- runOpaleyeT conn $ transaction $ do
    rems <- selectReminders
    (flip mapM) rems $ \(ReminderP rId rName rDesc rDT) -> do
      rEmails <- getReminderEmails rId
      return (rId, rName, rDesc, rDT, rEmails)
  return $ (flip map) reminders $ \( (rrID, rrName, rrDesc, rrDt, rrEmails) ) -> Reminder { reminderID = rrID,
                                                                                            reminderName = rrName,
                                                                                            reminderDescription = rrDesc,
                                                                                            reminderDateTime = rrDt,
                                                                                            reminderEmails = rrEmails
                                                                                          }
    where
      getReminderEmails :: Int -> Transaction [ String ]
      getReminderEmails reminderId = query $ reminderEmailsQuery reminderId

      reminderEmailsQuery :: Int -> Query ( (Column PGText) )
      reminderEmailsQuery reminderId = proc() -> do
        rems <- selectTable reminderTable -< ()
        restrict -< (remID rems) .== (pgInt4 reminderId)

        remEmail <- selectTable reminderEmailTable -< ()
        restrict -< (reReminderID remEmail) .== (remID rems)

        email <- selectTable emailTable -< ()
        restrict -< (reEmailID remEmail) .== (emID email)

        returnA -< (emEmail email)

      selectReminders :: Transaction [ ReminderP Int String String UTCTime ]
      selectReminders = query $ remindersQuery

      remindersQuery :: Query ReadReminder
      remindersQuery = proc() -> do
        rems <- selectTable reminderTable -< ()
        restrict -< (remDateTime rems) .>= (pgUTCTime beginDate)
        restrict -< (remDateTime rems) .<= (pgUTCTime endDate)
        -- restrict -< (remProcessed rems) .== (pgBool True)

        --remEmail <- selectTable reminderEmailTable -< ()
        --restrict -< (reReminderID remEmail) .== (remID rems)

        --email <- selectTable emailTable -< ()
        --restrict -< (reEmailID remEmail) .== (emID email)

        --returnA -< ( (remID rems), (remName rems), (remDescription rems), (remDateTime rems) )
        returnA -< (rems)
