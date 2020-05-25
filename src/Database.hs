{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Database where

import Control.Monad.Logger    (runStderrLoggingT)
import Control.Monad.Trans.Reader
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Data.UUID
import DatabaseConfig
import Data.Time.Clock

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    publicKey UUID sqltype=uuid   default=uuid_generate_v4()
    username String
    password String
    Primary publicKey
    deriving Show
    sessionId SessionId
Session
    key UUID sqltype=uuid default=uuid_generate_v4()
    expiresAt UTCTime
    Primary key
|]

-- authorId PersonId

connStr = "host=localhost dbname=test user=postgres password=test port=5432"

makePool :: IO ConnectionPool
makePool = runStderrLoggingT $ createPostgresqlPool connStr 10
