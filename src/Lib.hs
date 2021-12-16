module Lib
  ( simulate
  ) where

import           Control.Monad                  ( guard
                                                , join
                                                )
import qualified Data.List                     as List
import           Data.List                      ( group )
import qualified Data.Set                      as Set
import qualified Data.Heap                     as Heap
import           Data.Sort                      ( sort )
import           Debug.Trace                    ( trace )
import           System.Random.SplitMix         ( nextWord64 )
import           System.Random.SplitMix.Distributions
                                                ( gamma
                                                , sample
                                                , samples
                                                , uniformR
                                                , withGen
                                                , zipf
                                                )

clamp (min, max) v | v < min   = min
                   | v > max   = max
                   | otherwise = v

cycleWorkload range length = take length $ cycle [1 .. range]

uniformWorkload range length gen =
  map round $ samples length (fst $ nextWord64 gen) (uniformR 1 range)

zipfWorkload _     _     0      _   = []
zipfWorkload alpha range length gen = fst nextValue
  : zipfWorkload alpha range (length - 1) (snd nextValue)
 where
  nextValue = genNextValue gen
  genNextValue gen' = if fst candidate <= range
    then candidate
    else genNextValue (snd nextSeed)
   where
    candidate = (sample (fst nextSeed) (zipf alpha), snd nextSeed)
    nextSeed  = nextWord64 gen'

-- histogram $ zipfWorkload 1.5 1000 1000 (mkSMGen 12)

histogram l = mapM (putStrLn . encoded) runs
 where
  runs = (group . sort) l
  encoded r = replicate (length r) '*'

class Policy s where
    update :: s -> Int -> s
    evict :: s -> Int -> (Int, s)

newtype LRU = LRUList [Int]
newtype FIFO = FIFOList [Int]
newtype LFU = LFUHeap (Heap.MinPrioHeap Int Int)
newtype DLFU = DLFUHeap (Heap.MinPrioHeap Float (Int, Int, Int)) -- Format: (count (lastTime, totalTime, id))

instance Policy LRU where
  update (LRUList lruList) id = LRUList $ id : List.delete id lruList
  evict (LRUList lruList) id = (last lruList, LRUList $ id : init lruList)

instance Policy FIFO where
  update (FIFOList fifoList) id 
    | List.elem id fifoList = FIFOList fifoList
    | otherwise = FIFOList $ id : fifoList
  evict (FIFOList fifoList) id = (last fifoList, FIFOList $ id : init fifoList)

instance Policy LFU where
  update (LFUHeap lfuHeap) id
    | length idList == 1 = LFUHeap $ Heap.insert (p+1, id) updatedHeap
    | otherwise = LFUHeap $ Heap.insert (1, id) updatedHeap
      where 
        heapList = Heap.toList lfuHeap
        (idList, otherList) = List.partition (\(_, val) -> val == id) heapList
        [(p, _)] = idList
        updatedHeap = Heap.fromList otherList
  
  evict (LFUHeap lfuHeap) id = (evicted, LFUHeap $ Heap.insert (1, id) $ Heap.drop 1 lfuHeap)
    where [(_, evicted)] = Heap.take 1 lfuHeap

instance Policy DLFU where 
  update (DLFUHeap dlfuHeap) id
    | length idList == 1 = DLFUHeap $ Heap.insert (count, (totalT, totalT, id)) updatedHeap
    | otherwise = DLFUHeap $ Heap.insert (1.0, (1, 1, id)) dlfuHeap
      where 
        const = 1.0 / ( 0.0002 * ( log 2 ) )
        hList = Heap.toList dlfuHeap
        heapList = [ (c, (l, t+1, v)) | (c, (l, t, v)) <- hList ]
        (idList, otherList) = List.partition (\(_, (_, _, val)) -> val == id) heapList
        [(p, (lastT, totalT, _))] = idList
        count = p * const / (const + fromIntegral (totalT - lastT))
        updatedHeap = Heap.fromList otherList
  
  evict (DLFUHeap dlfuHeap) id = (evicted, DLFUHeap $ Heap.insert (1.0, (1, 1, id)) $ Heap.drop 1 dlfuHeap)
    where [(_, (_, _, evicted))] = Heap.take 1 dlfuHeap

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
                            (misses + 1)
    cacheMiss = tickSimulate restOfWorkload
                             policy'
                             (cacheContents', cacheSize)
                             (misses + 1)
    (evicted, policy') = evict policy nextTouch
    cacheContents'     = Set.insert nextTouch (Set.delete evicted cacheContents)
  tickSimulate [] _ _ misses = misses
