{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Arrows #-}

module Types where

import Data.Time
import GHC.Generics

data Reminder = Reminder { reminderID :: Int,
                           reminderName :: String,
                           reminderDescription :: String,
                           reminderDateTime :: LocalTime,
                           reminderEmails :: [String],
                           reminderProcessed :: Bool
                         } deriving (Eq, Show, Generic)
