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

type WriteReminder = ReminderP (Maybe (Column PGInt4)) (Column PGText) (Column PGText) (Column PGTimestamptz)
type ReadReminder = ReminderP (Column PGInt4) (Column PGText) (Column PGText) (Column PGTimestamptz)

makeAdaptorAndInstance "pReminder" ''ReminderP

reminderTable :: Table (WriteReminder) (ReadReminder)
reminderTable = Table "Reminders" $ pReminder ReminderP
  { remID = optional "ID"
  , remName = required "Name"
  , remDescription = required "Description"
  , remDateTime = required "ReminderDateTime"
  }

-- TOP LEVEL FUNCTIONS --

getReminders :: PSQL.Connection -> UTCTime -> UTCTime -> IO [Reminder]
getReminders conn beginDate endDate = do
  reminders <- runOpaleyeT conn $ transaction $ selectReminders
  return $ (flip map) reminders $ \(ReminderP remID remName remDescription remDateTime) -> Reminder { reminderID = remID, reminderName = remName, reminderDescription = remDescription,
                                                                                                      reminderDateTime = remDateTime }
    where
      selectReminders :: Transaction [ ReminderP Int String String UTCTime ]
      selectReminders = query $ remindersQuery

      remindersQuery :: Query ReadReminder
      remindersQuery = proc() -> do
        rems <- selectTable reminderTable -< ()
        restrict -< (remDateTime rems) .>= (pgUTCTime beginDate)
        restrict -< (remDateTime rems) .<= (pgUTCTime endDate)
        -- restrict -< (remProcessed rems) .== (pgBool True)
        returnA -< rems
