{-# LANGUAGE GADTs #-}

module Simulate
  ( simulate
  , simulateGraph
  , simulateGraphs
  , simulateGraphsSerially
  ) where

import qualified Control.Parallel.Strategies   as Strategies
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

simulateGraph :: [Int] -> Wrapper -> [Int] -> [Double]
simulateGraph workload policy sizes = Strategies.parMap
  Strategies.rpar
  (\size -> simulate workload policy size)
  sizes
  --Strategies.withStrategy (Strategies.parList Strategies.rdeepseq)
  --  $ map (simulate workload policy) sizes

simulateGraphs
  :: [(String, [Int])] -> [Wrapper] -> [Int] -> [(String, String, [Double])]
simulateGraphs workloads policies sizes = Strategies.parMap
  Strategies.rpar
  (\((workloadName, workloadAccesses), policy) ->
    (workloadName, show policy, simulateGraph workloadAccesses policy sizes)
  )
  [ (workload, policy) | workload <- workloads, policy <- policies ]

simulateGraphsSerially
  :: [(String, [Int])] -> [Wrapper] -> [Int] -> [(String, String, [Double])]
simulateGraphsSerially workloads policies sizes =
  [ (workloadName, show policy, simulateGraphSerially workloadAccesses policy)
  | (workloadName, workloadAccesses) <- workloads
  , policy                           <- policies
  ]
 where
  simulateGraphSerially workload policy = map (simulate workload policy) sizes
