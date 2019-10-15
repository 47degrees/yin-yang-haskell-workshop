{-# language OverloadedStrings #-}
module V2GreetSolution where

import Network.Simple.TCP

main :: IO ()
main = serve (Host "127.0.0.1") "8080" $ \(socket, _) -> do
         name <- recv socket 10000
         case name of
           Nothing -> return ()
           Just nm -> do let greeting = "Hello, " <> nm
                         send socket greeting
