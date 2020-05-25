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

module API where

import Data.Aeson                       
import Data.ByteString                  
import Data.Map                         
import qualified Data.Map            as Map
import Data.Proxy                       
import Data.Text                        
import GHC.Generics                     
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


-- | private data that needs protection
newtype PrivateData = PrivateData { ssshhh :: Text }
  deriving (Eq, Show, Generic)

instance ToJSON PrivateData

-- | public data that anyone can use.
newtype PublicData = PublicData { somedata :: Text }
  deriving (Eq, Show, Generic)

instance ToJSON PublicData
instance ToJSON User

-- | A user we'll grab from the database when we authenticate someone
newtype User = User { userName :: Text }
  deriving (Eq, Show, Generic)

-- | a type to wrap our public api
type PublicAPI = "login" :> QueryParam "username" Text :> QueryParam "password" Text :> Get '[JSON] (Headers '[Header "Set-Cookie" SetCookie] [PublicData])
            :<|> "test" :> Get '[JSON] [User]

-- | a type to wrap our private api
type PrivateAPI = Get '[JSON] PrivateData
-- | An account type that we "fetch from the database" after
-- performing authentication
newtype Account = Account { unAccount :: Text }

-- | A (pure) database mapping keys to accounts.
database :: Map ByteString Account
database = fromList [ ("key1", Account "Anne Briggs")
                    , ("key2", Account "Bruce Cockburn")
                    , ("key3", Account "Ghédalia Tazartès")
                    ]

-- | A method that, when given a password, will return a Account.
-- This is our bespoke (and bad) authentication logic.
lookupAccount :: ByteString -> Handler Account
lookupAccount key = case Map.lookup key database of
  Nothing -> throwError (err403 { errBody = "Invalid Cookie" })
  Just usr -> return usr

  --- | The auth handler wraps a function from Request -> Handler Account.
--- We look for a token in the request headers that we expect to be in the cookie.
--- The token is then passed to our `lookupAccount` function.
authHandler :: AuthHandler Request Account
authHandler = mkAuthHandler handler
  where
  maybeToEither e = maybe (Left e) Right
  throw401 msg = throwError $ err401 { errBody = msg }
  handler req = either throw401 lookupAccount $ do
    cookie <- maybeToEither "Missing cookie header" $ Prelude.lookup "cookie" $ requestHeaders req
    maybeToEither "Missing token in cookie" $ Prelude.lookup "servant-auth-cookie" $ parseCookies cookie

    -- | Our API, with auth-protection
type AuthGenAPI = "private" :> AuthProtect "cookie-auth" :> PrivateAPI
             :<|> PublicAPI

-- | A value holding our type-level API
genAuthAPI :: Proxy AuthGenAPI
genAuthAPI = Proxy

-- | We need to specify the data returned after authentication
type instance AuthServerData (AuthProtect "cookie-auth") = Account

-- | The context that will be made available to request handlers. We supply the
-- "cookie-auth"-tagged request handler defined above, so that the 'HasServer' instance
-- of 'AuthProtect' can extract the handler and run it on the request.
genAuthServerContext :: Context (AuthHandler Request Account ': '[])
genAuthServerContext = authHandler :. EmptyContext

-- | Our API, where we provide all the author-supplied handlers for each end
-- point. Note that 'privateDataFunc' is a function that takes 'Account' as an
-- argument. We dont' worry about the authentication instrumentation here,
-- that is taken care of by supplying context
genAuthServer :: ServerT AuthGenAPI App
genAuthServer =
  let privateDataFunc (Account name) = do
      pool <- db <$> ask
      return (PrivateData ("this is a secret: " <> name))
  in privateDataFunc :<|> login :<|> getUsers

login :: Maybe Text -> Maybe Text -> App (Headers '[Header "Set-Cookie" SetCookie] [PublicData])
login (Just name) (Just pw) = do
  pool <- db <$> ask
  pure $ addHeader (def { setCookieName = "servant-auth-cookie", setCookieValue = "key3", setCookieHttpOnly = True }) [PublicData ("Hello: " <> name)]
login _ _ = throwError err401

getUsers :: App [User]
getUsers = do
  pool <- db <$> ask
  return [User "username"]

  -- | run our server
genAuthMain :: IO ()
genAuthMain = do
  pool <- makePool
  runSqlPool (runMigration migrateAll) pool
  run 8080 $ app $ AppEnv { db = pool }

app s = 
  serveWithContext genAuthAPI genAuthServerContext $
    hoistServerWithContext genAuthAPI (Proxy :: Proxy (AuthHandler Request Account ': '[]))
      (flip runReaderT s) genAuthServer

data AppEnv = AppEnv { db :: ConnectionPool }
type App = ReaderT AppEnv Handler

nt :: AppEnv -> App a -> Handler a
nt s x = runReaderT x s


