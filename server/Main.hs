{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
module Main (main) where

import           Data.Text                      (Text)
import           Network.Wai
import           Network.Wai.Application.Static as Static
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WebSockets
import           Network.WebSockets             as WS
import           WaiAppStatic.Types             (unsafeToPiece)

main :: IO ()
main = do
    runSettings
      (setPort 3301 defaultSettings)
      $ websocketsOr WS.defaultConnectionOptions wsServer staticServer

staticServer :: Application
staticServer = Static.staticApp
    (Static.defaultWebAppSettings "frontend")
      { ssIndices = [unsafeToPiece "index.html"] }

wsServer :: WS.ServerApp
wsServer r = do
    conn <- WS.acceptRequest r
    WS.sendTextData conn ("ping" :: Text)
    WS.forkPingThread conn 30
