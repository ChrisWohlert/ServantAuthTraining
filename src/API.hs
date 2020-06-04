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
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}

module API where

import Type.Reflection
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
import Config
import Data.UUID
import GHC.TypeLits



-- | private data that needs protection
newtype PrivateData = PrivateData { ssshhh :: Text }
  deriving (Eq, Show, Generic)

instance ToJSON PrivateData

-- | public data that anyone can use.
newtype PublicData = PublicData { somedata :: Text }
  deriving (Eq, Show, Generic)

instance ToJSON PublicData
instance ToJSON User


newtype User = User { userName :: Text }
  deriving (Eq, Show, Generic)


type PublicAPI = "login" :> QueryParam "username" Text :> QueryParam "password" Text :> Get '[JSON] (Headers '[Header "Set-Cookie" SetCookie] [PublicData])
            :<|> "test" :> Get '[JSON] [User]

type GetList a = Get '[JSON] [a]
type GetOne a i = Capture "id" i :> Get '[JSON] a
type CreateNew a = ReqBody '[JSON] a :> Post '[JSON] NoContent

type Crud (name :: Symbol) a i = name :> 
  (    GetList a
  :<|> GetOne a i
  :<|> CreateNew a
  )

data Book = Book { isbn :: String } deriving (Show, Generic)

instance ToJSON Book
instance FromJSON Book

type Redirect a = Headers '[Header "location" a] NoContent

type PrivateAPI = Get '[JSON] PrivateData



lookupAccount :: ConnectionPool -> ByteString -> Handler User
lookupAccount pool sessionId = do
  maybeUser <- runWithPool pool (selectFirst [] [])
  case maybeUser of
    Nothing -> throwError (err403 { errBody = "Invalid Cookie" })
    (Just u) -> return $ toUser u


authHandler :: ConnectionPool -> AuthHandler Request User
authHandler pool = mkAuthHandler handler
  where
  maybeToEither e = maybe (Left e) Right
  throw401 msg = throwError $ err401 { errBody = msg }
  handler req = either throw401 (lookupAccount pool) $ do
    cookie <- maybeToEither "Missing cookie header" $ Prelude.lookup "cookie" $ requestHeaders req
    maybeToEither "Missing token in cookie" $ Prelude.lookup "servant-auth-cookie" $ parseCookies cookie


type AuthGenAPI = Crud "book" Book UUID


genAuthAPI :: Proxy AuthGenAPI
genAuthAPI = Proxy


type instance AuthServerData (AuthProtect "cookie-auth") = User


genAuthServerContext :: ConnectionPool -> Context (AuthHandler Request User ': '[])
genAuthServerContext pool = authHandler pool :. EmptyContext


genAuthServer :: ServerT AuthGenAPI App
genAuthServer = crud (\ (UserDatabaseModel k u p) -> Book "isbn")

getBooks f = do
  elements <- runQuery (selectList [] [])
  return $ Prelude.map f . Prelude.map (\ (Entity k v) -> v) $ elements

editBook :: UUID -> App Book
editBook _ = return (Book "dsa")
  
createBook :: Book -> App NoContent
createBook b = return NoContent

crud f = getBooks f :<|> editBook :<|> createBook

redirect link = return $ addHeader link NoContent

privateDataFunc (User name) = do
  pool <- asks db
  return (PrivateData ("this is a secret: " <> name))

login :: Maybe Text -> Maybe Text -> App (Headers '[Header "Set-Cookie" SetCookie] [PublicData])
login (Just name) (Just pw) = do
  pool <- db <$> ask
  pure $ addHeader (def { setCookieName = "servant-auth-cookie", setCookieValue = "key3", setCookieHttpOnly = True }) [PublicData ("Hello: " <> name)]
login _ _ = throwError err401

getUsers :: App [User]
getUsers = do
  users <- runQuery (selectList [] [])
  return $ Prelude.map toUser users

toUser :: Entity UserDatabaseModel -> User
toUser (Entity k (UserDatabaseModel key name pw)) = User (Data.Text.pack name)

  -- | run our server
genAuthMain :: IO ()
genAuthMain = do
  pool <- makePool
  runSqlPool (runMigration migrateAll) pool
  run 8080 $ app $ AppEnv { db = pool }

app cfg = 
  serveWithContext genAuthAPI (genAuthServerContext (db cfg)) $
    hoistServerWithContext genAuthAPI (Proxy :: Proxy (AuthHandler Request User ': '[]))
      (flip runReaderT cfg) genAuthServer


nt :: AppEnv -> App a -> Handler a
nt s x = runReaderT x s












-- GENERICS

data InputField = InputField { iptype :: String, ipValue :: String, ipName :: String } 
                | LabelField LabelText InputField 
                deriving (Show) 

class ToForm' f where
  toForm' :: f p -> [InputField]

class ToForm a where
  toForm :: a -> [InputField]
  default toForm :: (Generic a, ToForm' (Rep a)) => a -> [InputField]
  toForm x = toForm' (from x)

instance ToForm' V1 where
  toForm' = undefined

instance ToForm' U1 where
  toForm' U1 = []

instance (ToForm' f, ToForm' g) => ToForm' (f :+: g) where
  toForm' (L1 x) = toForm' x
  toForm' (R1 x) = toForm' x

instance (ToForm' f, ToForm' g) => ToForm' (f :*: g) where
  toForm' (x :*: y) = toForm' x ++ toForm' y

instance (ToForm c) => ToForm' (K1 i c) where
  toForm' (K1 x) = toForm x

instance (ToForm' f) => ToForm' (M1 i t f) where
  toForm' (M1 x) = toForm' x


data LabelText = LabelUsername | PasswordLabel deriving (Show)

data FormInput a = FormInput LabelText a deriving (Show, Generic)

data Test = TestUser { tusername :: FormInput String, tpassword :: String, tage :: Int, tsession :: Session } deriving (Show, Generic)

data Session = Session { sid :: String, sexpires :: String } deriving (Show, Generic)

instance ToForm Char where
  toForm x = []

instance ToForm Test
instance ToForm Session

instance {-# Overlaps #-} (Selector s, ToInputField t) => ToForm' (M1 S s (K1 R t)) where
  toForm' x@(M1 e) = [(toInputField (selName x) (unK1 e))]

class ToInputField a where
  toInputField :: String -> a -> InputField

instance ToInputField String where
  toInputField n v = InputField "text" v n

instance (ToInputField a) => ToInputField (FormInput a) where
  toInputField n (FormInput l v) = LabelField l (toInputField n v)

instance ToInputField Int where
  toInputField n v = InputField "number" (show v) n
  
instance ToInputField Session where
  toInputField n (Session sid sexpires) = InputField "text" sid n

showTest = toForm $ TestUser (FormInput LabelUsername "a") "b" 4 (Session "c" "d")