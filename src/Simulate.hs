{-# LANGUAGE GADTs #-}

module Simulate
  ( simulate
  , simulatePoint
  , simulateGraph
  , simulateGraphs
  ) where

import qualified Control.Parallel.Strategies   as Strategies
import qualified Data.List                     as List
import qualified Data.Set                      as Set

import           Policies                       ( Policy
                                                , Wrapper(..)
                                                , evict
                                                , update
                                                )

simulate :: Policy p => [Int] -> p -> Int -> Double
simulate workload policyStart size =
  fromIntegral (tickSimulate workload policyStart (Set.empty, size) 0 :: Int)
    / fromIntegral (length workload)
 where
  tickSimulate (nextTouch : restOfWorkload) policy cache@(cacheContents, cacheSize) misses
    | Set.member nextTouch cacheContents
    = cacheHit
    | length cacheContents < cacheSize
    = cacheAdd
    | otherwise
    = cacheMiss
   where
    cacheHit =
      tickSimulate restOfWorkload (update policy nextTouch) cache misses
    cacheAdd = tickSimulate restOfWorkload
                            (update policy nextTouch)
                            (Set.insert nextTouch cacheContents, cacheSize)
                            (misses + 1)
    cacheMiss = tickSimulate restOfWorkload
                             policy'
                             (cacheContents', cacheSize)
                             (misses + 1)
    (evicted, policy') = evict policy nextTouch
    cacheContents'     = Set.insert nextTouch (Set.delete evicted cacheContents)
  tickSimulate [] _ _ misses = misses

simulatePoint
  :: Int -> Wrapper -> (Set.Set Int, Int) -> Int -> (Wrapper, Set.Set Int, Int)
simulatePoint nextTouch policy (cacheContents, cacheSize) misses
  | Set.member nextTouch cacheContents = cacheHit
  | length cacheContents < cacheSize   = cacheAdd
  | otherwise                          = cacheMiss
 where
  cacheHit = (update policy nextTouch, cacheContents, misses)
  cacheAdd =
    (update policy nextTouch, Set.insert nextTouch cacheContents, misses + 1)
  cacheMiss          = (policy', cacheContents', misses + 1)
  (evicted, policy') = evict policy nextTouch
  cacheContents'     = Set.insert nextTouch (Set.delete evicted cacheContents)

simulateGraph :: [Int] -> [(Wrapper, Set.Set Int, Int)] -> Int -> [Int]
simulateGraph [] policiesData _ = [ 0 | _ <- policiesData ]
simulateGraph (w : workload) policiesData size
  | null workload = misses
  | otherwise     = simulateGraph workload pData size
 where
  pData = Strategies.parMap
    Strategies.rpar
    (\(p, cContents, m) -> simulatePoint w p (cContents, size) m)
    policiesData
  misses = [ m | (_, _, m) <- pData ]

simulateGraphs :: [[Int]] -> [Wrapper] -> [Int] -> [[Double]]
simulateGraphs workloads policies sizes = Strategies.parMap Strategies.rpar
                                                            simulateSizeGraph
                                                            workloads
 where
  simulateSizeGraph w = List.concat $ Strategies.parMap
    Strategies.rpar
    ( map (\pt -> fromIntegral pt / fromIntegral (length w))
    . simulateGraph w policiesData
    )
    sizes
  policiesData = [ (p, Set.empty, 0) | p <- policies ]
