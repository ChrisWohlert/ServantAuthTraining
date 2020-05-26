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
import Config
import Control.Monad.IO.Class (liftIO)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
UserDatabaseModel sql=user
    key UUID sqltype=uuid   default=uuid_generate_v4() sql=id
    username String
    password String
    Primary key
    deriving Show
SessionDatabaseModel sql=session
    key UUID sqltype=uuid default=uuid_generate_v4() sql=id
    expiresAt UTCTime
    Primary key
    userId UserDatabaseModelId
    UniqueUserId
|]

-- authorId PersonId

connStr = "host=localhost dbname=test user=postgres password=test port=5432"

makePool :: IO ConnectionPool
makePool = runStderrLoggingT $ createPostgresqlPool connStr 10


runQuery query = do
    pool <- asks db
    liftIO $ runSqlPool query pool

runWithPool pool query  = liftIO $ runSqlPool query pool