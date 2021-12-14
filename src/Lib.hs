module Lib
  ( someFunc
  ) where

import qualified Data.List                     as List
import qualified Data.Set                      as Set
import           Debug.Trace                    ( trace )

someFunc :: IO ()
someFunc = putStrLn "someFunc"

class Policy s where
    update :: s -> Int -> s
    evict :: s -> Int -> (Int, s)

newtype LRU = LRUList [Int]

instance Policy LRU where
  update (LRUList lruList) id = LRUList $ id : List.delete id lruList
  evict (LRUList lruList) id = (last lruList, LRUList $ id : init lruList)

simulate :: Policy p => [Int] -> p -> Int -> Double
simulate workload policy size =
  fromIntegral (tickSimulate workload policy (Set.empty, size) 0)
    / fromIntegral (length workload)
 where
  tickSimulate (nextTouch : restOfWorkload) policy cache@(cacheContents, cacheSize) misses
    | Set.member nextTouch cacheContents
    = trace ("Hit" ++ show nextTouch) cacheHit
    | length cacheContents < cacheSize
    = trace ("Add" ++ show nextTouch) cacheAdd
    | otherwise
    = trace ("Miss" ++ show nextTouch) cacheMiss
   where
    cacheHit =
      tickSimulate restOfWorkload (update policy nextTouch) cache misses
    cacheAdd = tickSimulate restOfWorkload
                            (update policy nextTouch)
                            (Set.insert nextTouch cacheContents, cacheSize)
                            misses
    cacheMiss = tickSimulate restOfWorkload
                             policy'
                             (cacheContents', cacheSize)
                             (misses + 1)
    (evicted, policy') = evict policy nextTouch
    cacheContents'     = Set.insert nextTouch (Set.delete evicted cacheContents)
  tickSimulate [] _ _ misses = misses
