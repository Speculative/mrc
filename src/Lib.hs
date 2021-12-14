module Lib
  ( someFunc
  ) where

import           Data.Set                       ( Set
                                                , delete
                                                , empty
                                                , insert
                                                , member
                                                )

someFunc :: IO ()
someFunc = putStrLn "someFunc"

class Policy s where
    update :: s -> Int -> s
    evict :: s -> Int -> (Int, s)

newtype LRU = LRUList [Int]

instance Policy LRU where
  update lruList id = lruList
  evict lruList id = (0, lruList)

simulate :: Policy p => [Int] -> p -> Int -> Double
simulate workload policy size =
  fromIntegral (tickSimulate workload policy (empty, size) 0)
    / fromIntegral (length workload)
 where
  tickSimulate (nextTouch : restOfWorkload) policy cache@(cacheContents, cacheSize) misses
    = if member nextTouch cacheContents then cacheHit else cacheMiss
   where
    cacheHit =
      tickSimulate restOfWorkload (update policy nextTouch) cache misses
    cacheMiss = tickSimulate restOfWorkload
                             policy'
                             (cacheContents', cacheSize)
                             (misses + 1)
    (evicted, policy') = evict policy nextTouch
    cacheContents'     = insert nextTouch (delete evicted cacheContents)
  tickSimulate [] _ _ misses = misses
