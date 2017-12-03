{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import           Control.Concurrent            (forkIO, killThread)
import           Control.Monad
import           Control.Monad.IO.Class        (liftIO)
import           Data.Aeson                    (decode, encode)
import           Data.ByteString.Lazy          (ByteString)
import           Data.Maybe
import           Data.Monoid
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.Encoding       as TL
import           Moba.Client                   (fight)
import           Moba.Internal
import           Network.Socket                (withSocketsDo)
import           Network.WebSockets
import           Network.WebSockets            (WebSocketsData (..))
import           Options.Applicative
import           System.Console.Haskeline
import           System.Log.FastLogger
import           Text.InterpolatedString.Perl6 (q)

data AppOptions = AppOptions
                { url     :: String
                , port    :: Int
                , game    :: String
                , token   :: String
                , picking :: String
                , ops     :: Int
                }

instance WebSocketsData LogStr where
    fromLazyByteString = toLogStr
    toLazyByteString = toLazyByteString . fromLogStr

instance WebSocketsData FromServer where
    fromLazyByteString = fromJust . decode
    toLazyByteString = encode

instance WebSocketsData ToServer where
    fromLazyByteString = fromJust . decode
    toLazyByteString = encode

options :: Parser AppOptions
options = AppOptions
      <$> strOption   ( long "url"     <> short 'u'     <> help "服务器地址")
      <*> option auto ( long "port"    <> short 'p'     <> help "服务器端口")
      <*> strOption   ( long "game"    <> short 'g'     <> help "GameID, e.g. 00v00")
      <*> strOption   ( long "token"   <> short 'k'     <> help "Token")
      <*> strOption   ( long "picking" <> value pickCmd <> help "选择英雄")
      <*> option auto ( long "ops"     <> short 'f'     <> help "每次更新响应命令数，默认8条")

pickCmd :: String
pickCmd = [q|{"type":"pickHero","heros":["warrior","warrior","shooter","shooter","shooter"]}|]

main :: IO ()
main = doMain =<< execParser opts
  where
    opts = info (options <**> helper)
      ( fullDesc <> progDesc "客户端程序" <> header "client" )

doMain :: AppOptions -> IO ()
doMain a@(AppOptions{..}) = do
  (log, _) <-  newFastLogger (LogFile logfile 4096)

  runClient url port "/" $ \conn -> do
    threadId <- forkIO . forever $ do
      msg :: ByteString
        <- receiveData conn
      log $ toLogStr $ msg <> "\n"
      case (fromJust $ decode msg) of
        Picking        -> do sendTextData conn (T.pack picking)
                             putStrLn $ "> Picking"
                             putStrLn $ "> " ++ picking
        CmdError s     -> putStrLn $ "> " ++ s
        Loading camp _ -> putStrLn $ "> " ++ show camp
        Start          -> putStrLn "> start"
        End            -> putStrLn "> end"
        update         ->
          case fight update of
            -- do nothing
            Nothing  -> return ()
            -- take action
            Just cmds    -> do
              forM_ (take ops cmds) $ \cmd -> do
                sendTextData conn cmd

    -- 加入游戏
    sendTextData conn $ encode $ Join game token
    -- 交互模式
    runInputT defaultSettings $ loop conn
    killThread threadId
  where
    loop :: Connection -> InputT IO ()
    loop conn = do
      minput <- getInputLine "> "
      case minput of
        Nothing -> close conn
        Just "" -> loop conn
        Just input -> do
          liftIO $ sendTextData conn $ T.pack input
          loop conn
    close conn = liftIO $ sendClose conn ("" :: Text)
    logfile = FileLogSpec "./moba.log" (1024 * 1024 * 5) 1
