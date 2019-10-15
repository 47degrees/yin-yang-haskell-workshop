{-# language OverloadedStrings #-}
module V1SayHello where

import Network.Simple.TCP

main :: IO ()
main = serve (Host "127.0.0.1") "8080" $ \(socket, _) -> do
         send socket "Hello\n"
