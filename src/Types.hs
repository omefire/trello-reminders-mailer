{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Arrows #-}

module Types where

import Data.Time
import GHC.Generics

data Reminder = Reminder { reminderID :: Int,
                           reminderName :: String,
                           reminderDescription :: String,
                           reminderDateTime :: UTCTime,
                           reminderEmails :: [String]
                         } deriving (Eq, Show, Generic)
