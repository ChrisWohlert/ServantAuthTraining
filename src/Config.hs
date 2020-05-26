

module Config where

import Servant                
import Database.Persist.Sql
import Control.Monad.Trans.Reader        


data AppEnv = AppEnv { db :: ConnectionPool }
type App = ReaderT AppEnv Handler