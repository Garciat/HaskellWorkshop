{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

import           Control.Monad.Catch (throwM)
import           Data.Proxy
import           GHC.Generics
import           System.IO

import           Data.Aeson
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

-- * api

type ItemApi =
  "item" :> Get '[JSON] [Item] :<|>
  "item" :> Capture "itemId" Integer :> Get '[JSON] Item

itemApi :: Proxy ItemApi
itemApi = Proxy

-- * app

server :: Server ItemApi
server =
  getItems :<|>
  getItemById

getItems :: Handler [Item]
getItems = return [exampleItem]

getItemById :: Integer -> Handler Item
getItemById itemId =
  case itemId of
    0 -> return exampleItem
    _ -> throwM err404

exampleItem :: Item
exampleItem = Item 0 "example item"

-- * item

data Item
  = Item {
    itemId :: Integer,
    itemText :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Item
instance FromJSON Item

(|>) = flip ($)

----

mkApp :: IO Application
mkApp = return $ serve itemApi server

main :: IO ()
main = do
  let port = 3000
      settings =
        defaultSettings
        |> setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port))
        |> setPort port
  app <- mkApp
  runSettings settings app
