{-# language OverloadedStrings #-}
{-# language DeriveGeneric #-}
module V3JsonCommands where

import Data.Aeson
import Data.ByteString.Char8 (pack)
import GHC.Generics
import Network.Simple.TCP

data Command = Command { command :: String, name :: Maybe String }
             deriving (Eq, Show, Generic)
            
instance ToJSON Command
instance FromJSON Command

main :: IO ()
main = serve (Host "127.0.0.1") "8080" $ \(socket, _) -> do
         content <- recv socket 10000
         case content of
           Nothing -> putStrLn "connection closed"
           Just content -> do
            let cmd = decodeStrict content
            case cmd of
              Just (Command "greet" (Just name)) -> do
                let greeting = "Hello, " <> pack name
                send socket greeting
              _ -> putStrLn "unknown command"

main2 :: IO ()
main2 = serve (Host "127.0.0.1") "8080" $ \(socket, _) -> do
         content <- recv socket 10000
         case content >>= decodeStrict of
            Just (Command "greet" (Just name)) -> do
              let greeting = "Hello, " <> pack name
              send socket greeting
            _ -> putStrLn "unknown command"