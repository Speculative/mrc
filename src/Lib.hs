{-# LANGUAGE GADTs #-}

module Lib
  ( simulate, simulateGraphs
  ) where

import qualified Data.Heap                     as Heap
import qualified Data.List                     as List
import qualified Data.Set                      as Set
import qualified Control.Parallel.Strategies   as Strategies
import           Debug.Trace                    ( trace )

class Policy s where
    update :: s -> Int -> s
    evict :: s -> Int -> (Int, s)

newtype LRU = LRUList [Int]
newtype FIFO = FIFOList [Int]
newtype LFU = LFUHeap (Heap.MinPrioHeap Int Int)
newtype DLFU = DLFUHeap (Heap.MinPrioHeap Float (Int, Int, Int)) -- Format: (count (lastTime, totalTime, id))
data Wrapper = LRU LRU | FIFO FIFO | LFU LFU | DLFU DLFU

instance Policy Wrapper where
  update (LRU l) wid = LRU $ update l wid
  update (FIFO l) wid = FIFO $ update l wid
  update (LFU l) wid = LFU $ update l wid
  update (DLFU l) wid = DLFU $ update l wid

  evict (LRU l) wid = (e, LRU p)
    where (e, p) = evict l wid
  evict (FIFO l) wid = (e, FIFO p)
    where (e, p) = evict l wid
  evict (LFU l) wid = (e, LFU p)
    where (e, p) = evict l wid
  evict (DLFU l) wid = (e, DLFU p)
    where (e, p) = evict l wid

instance Policy LRU where
  update (LRUList lruList) wid = LRUList $ wid : List.delete wid lruList
  evict (LRUList lruList) wid = (last lruList, LRUList $ wid : init lruList)

instance Policy FIFO where
  update (FIFOList fifoList) wid | wid `elem` fifoList = FIFOList fifoList
                                | otherwise = FIFOList $ wid : fifoList
  evict (FIFOList fifoList) wid = (last fifoList, FIFOList $ wid : init fifoList)

instance Policy LFU where
  update (LFUHeap lfuHeap) wid
    | length idList == 1 = LFUHeap $ Heap.insert (p + 1, wid) updatedHeap
    | otherwise          = LFUHeap $ Heap.insert (1, wid) updatedHeap
   where
    heapList            = Heap.toList lfuHeap
    (idList, otherList) = List.partition (\(_, val) -> val == wid) heapList
    [(p, _)]            = idList
    updatedHeap         = Heap.fromList otherList

  evict (LFUHeap lfuHeap) wid =
    (evicted, LFUHeap $ Heap.insert (1, wid) $ Heap.drop 1 lfuHeap)
    where [(_, evicted)] = Heap.take 1 lfuHeap

instance Policy DLFU where
  update (DLFUHeap dlfuHeap) wid
    | length idList == 1 = DLFUHeap
    $ Heap.insert (count, (totalT, totalT, wid)) updatedHeap
    | otherwise = DLFUHeap $ Heap.insert (1.0, (1, 1, wid)) dlfuHeap
   where
    decay    = 1.0 / (0.0002 * log 2)
    hList    = Heap.toList dlfuHeap
    heapList = [ (c, (l, t + 1, v)) | (c, (l, t, v)) <- hList ]
    (idList, otherList) =
      List.partition (\(_, (_, _, val)) -> val == wid) heapList
    [(p, (lastT, totalT, _))] = idList
    count = p * decay / (decay + fromIntegral (totalT - lastT))
    updatedHeap = Heap.fromList otherList

  evict (DLFUHeap dlfuHeap) wid =
    (evicted, DLFUHeap $ Heap.insert (1.0, (1, 1, wid)) $ Heap.drop 1 dlfuHeap)
    where [(_, (_, _, evicted))] = Heap.take 1 dlfuHeap

simulate :: Policy p => [Int] -> p -> Int -> Double
simulate workload policyStart size =
  fromIntegral (tickSimulate workload policyStart (Set.empty, size) 0)
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
                            (misses + 1)
    cacheMiss = tickSimulate restOfWorkload
                             policy'
                             (cacheContents', cacheSize)
                             (misses + 1)
    (evicted, policy') = evict policy nextTouch
    cacheContents'     = Set.insert nextTouch (Set.delete evicted cacheContents)
  tickSimulate [] _ _ misses = misses

simulatePoint :: Int -> Wrapper -> (Set.Set Int, Int) -> Int -> (Wrapper, Set.Set Int, Int)
simulatePoint nextTouch policy (cacheContents, cacheSize) misses
    | Set.member nextTouch cacheContents = cacheHit
    | length cacheContents < cacheSize = cacheAdd
    | otherwise = cacheMiss
   where
    cacheHit = (update policy nextTouch, cacheContents, misses)
    cacheAdd = (update policy nextTouch, Set.insert nextTouch cacheContents, misses + 1)
    cacheMiss = (policy', cacheContents', misses + 1)
    (evicted, policy') = evict policy nextTouch
    cacheContents' = Set.insert nextTouch (Set.delete evicted cacheContents)

simulateGraph :: [Int] -> [(Wrapper, Set.Set Int, Int)] -> Int -> [Int]
simulateGraph [] policiesData _ = [0 | _ <- policiesData]
simulateGraph (w:workload) policiesData size
  | null workload = misses
  | otherwise = simulateGraph workload pData size
  where
    pData = Strategies.parMap Strategies.rpar (\(p, cContents, m) -> simulatePoint w p (cContents, size) m) policiesData
    misses = [m | (_, _, m) <- pData]

simulateGraphs :: [[Int]] -> [Wrapper] -> [Int] -> [[[Double]]]
simulateGraphs workloads policies = Strategies.parMap Strategies.rpar simulateSizeGraphs
  where
    simulateSizeGraphs s = Strategies.parMap Strategies.rpar simulateSizeGraph workloads
      where
        simulateSizeGraph w = List.concat $ Strategies.parMap Strategies.rpar (map (\pt -> fromIntegral pt / fromIntegral (length w)) . simulateGraph w policiesData) [1..s]
    policiesData = [(p,  Set.empty, 0) | p <- policies]
