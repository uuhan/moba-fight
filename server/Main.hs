{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
module Main (main) where

import           Data.Text          (Text)
import           Yesod.Core         as Y
import           Yesod.Core.Handler as Y
import           Yesod.WebSockets   as Y

data Server = Server
mkYesod "Server" [parseRoutes|
/   RootR   GET
|]

instance Yesod Server

serverApp :: WebSocketsT Handler ()
serverApp = do
    sendTextData ("todo" :: Text)

getRootR :: Handler Html
getRootR = do
    webSockets serverApp
    defaultLayout $
      toWidget [hamlet|<div>server</div>|]

main :: IO ()
main = do
    Y.warp 3301 Server
