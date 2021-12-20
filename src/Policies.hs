{-# LANGUAGE GADTs #-}

module Policies
  ( LRU(..)
  , FIFO(..)
  , LFU(..)
  , DLFU(..)
  , Wrapper(..)
  , Policy
  , update
  , evict
  ) where

import qualified Data.Heap                     as Heap
import qualified Data.List                     as List

class Policy s where
    update :: s -> Int -> s
    evict :: s -> Int -> (Int, s)

newtype LRU = LRUList [Int]
newtype FIFO = FIFOList [Int]
newtype LFU = LFUHeap (Heap.MinPrioHeap Int Int)
newtype DLFU = DLFUHeap (Heap.MinPrioHeap Float (Int, Int, Int)) -- Format: (count (lastTime, totalTime, id))
data Wrapper = LRU LRU | FIFO FIFO | LFU LFU | DLFU DLFU

instance Policy Wrapper where
  update (LRU  l) wid = LRU $ update l wid
  update (FIFO l) wid = FIFO $ update l wid
  update (LFU  l) wid = LFU $ update l wid
  update (DLFU l) wid = DLFU $ update l wid

  evict (LRU  l) wid = (e, LRU p) where (e, p) = evict l wid
  evict (FIFO l) wid = (e, FIFO p) where (e, p) = evict l wid
  evict (LFU  l) wid = (e, LFU p) where (e, p) = evict l wid
  evict (DLFU l) wid = (e, DLFU p) where (e, p) = evict l wid

instance Show Wrapper where
  show (LRU _) = "LRU"
  show (FIFO _) = "FIFO"
  show (LFU _) = "LFU"
  show (DLFU _) = "DLFU"

instance Policy LRU where
  update (LRUList lruList) wid = LRUList $ wid : List.delete wid lruList
  evict (LRUList lruList) wid = (last lruList, LRUList $ wid : init lruList)

instance Policy FIFO where
  update (FIFOList fifoList) wid | wid `elem` fifoList = FIFOList fifoList
                                 | otherwise = FIFOList $ wid : fifoList
  evict (FIFOList fifoList) wid =
    (last fifoList, FIFOList $ wid : init fifoList)

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
