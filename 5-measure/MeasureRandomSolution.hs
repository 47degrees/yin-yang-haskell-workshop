module MeasureRandomSolution where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad (replicateM)
import System.Random

import Criterion.Main

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

throwCoinList :: Int -> IO Int
throwCoinList n = do
  lst <- replicateM n randomIO
  return $ count id lst

throwCoinLoop :: Int -> IO Int
throwCoinLoop n = go n 0
  where go 0 acc = return acc
        go n acc = do
          r <- randomIO
          if r then go (n-1) (acc+1)
               else go (n-1) acc

throwCoinListAsync :: Int -> IO Int
throwCoinListAsync n = do
  lst <- replicateConcurrently n randomIO
  return $ count id lst

throwCoinSTM :: Int -> IO Int
throwCoinSTM n = do 
  v <- newTVarIO 0
  replicateConcurrently_ n $ do
    r <- randomIO
    if r then atomically $ modifyTVar v (+1)
          else return ()
  readTVarIO v

main :: IO ()
main = defaultMain [ bench "list"  $ whnfIO (throwCoinList 10000)
                   , bench "loop"  $ whnfIO (throwCoinLoop 10000)
                   , bench "async" $ whnfIO (throwCoinListAsync 10000)
                   , bench "stm"   $ whnfIO (throwCoinSTM 10000) ]