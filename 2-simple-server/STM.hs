{-# language OverloadedStrings #-}
{-# language DeriveGeneric #-}
module STM where

import Control.Concurrent.STM

import Data.Aeson
import Data.ByteString.Char8 (pack)
import GHC.Generics
import Network.Simple.TCP

data Command = Command { command :: String, value :: Maybe String }
             deriving (Eq, Show, Generic)
            
instance ToJSON Command
instance FromJSON Command

main :: IO ()
main = do
  v <- newTVarIO "hello"
  serve (Host "127.0.0.1") "8080" $ \(socket, _) -> do
         content <- recv socket 10000
         case content >>= decodeStrict of
            Just (Command "read" _) -> do
              currentValue <- readTVarIO v
              send socket (pack currentValue)
            Just (Command "write" (Just newValue)) -> do
              oldValue <- atomically $ do
                old <- readTVar v
                writeTVar v newValue
                return old
              send socket (pack oldValue)
            _ -> putStrLn "unknown command"