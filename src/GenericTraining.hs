{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveDataTypeable #-}


module GenericTraining where

import Type.Reflection
import Data.Aeson                       
import Data.ByteString                  
import Data.Map                         
import qualified Data.Map            as Map
import Data.Proxy                       
import Data.Text                        
import qualified GHC.Generics                  as G              
import Network.Wai                      
import Network.Wai.Handler.Warp         
import Servant.API                      
import Servant.API.BasicAuth            
import Servant.API.Experimental.Auth    
import Servant                          
import Servant.Server                   
import Servant.Server.Experimental.Auth
import Web.Cookie                      
import Database
import Database.Persist.Sql
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)
import Config
import Data.UUID
import GHC.TypeLits              
import GHC.Generics       
import Data.Data
import Data.Either
import Web.FormUrlEncoded

data Test = TestUser { tusername :: String, tpassword :: String, tage :: Int, tsesssion :: Session } deriving (Show, Generic)

data Session = Session { sid :: String, sexpires :: String } deriving (Show, Generic)

instance FromForm Test

instance FromHttpApiData Session where
  parseQueryParam f = Session
    <$> parseUnique "sid" f
    <*> parseUnique "sexpires" f