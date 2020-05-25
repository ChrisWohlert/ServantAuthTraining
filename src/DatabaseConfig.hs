{-# LANGUAGE OverloadedStrings          #-}

module DatabaseConfig where

import qualified Data.ByteString.Char8 as B8
import Control.Monad.Trans.Reader
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Data.UUID
import Data.Time.Clock
import Web.PathPieces
import qualified Data.Text as T

instance PersistField UUID where
  toPersistValue = PersistDbSpecific . B8.pack . toString
  fromPersistValue (PersistDbSpecific t) =
    case fromString $ B8.unpack t of
      Just x -> Right x
      Nothing -> Left "Invalid UUID"
  fromPersistValue _ = Left "Not PersistDBSpecific"

instance PersistFieldSql UUID where
  sqlType _ = SqlOther "uuid"

instance PathPiece UUID where
  toPathPiece = T.pack . toString
  fromPathPiece = fromString . T.unpack