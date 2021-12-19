module Main where

import qualified Data.Heap                     as Heap
import           Lib                            ( DLFU(..)
                                                , FIFO(..)
                                                , LFU(..)
                                                , LRU(..)
                                                , Wrapper(..)
                                                , arcTraceWorkload
                                                , arcTraces
                                                , simulateGraphs
                                                , uniformWorkload
                                                , zipfWorkload
                                                )
import           System.Random.SplitMix         ( initSMGen )

main :: IO ()
main = do
  gen <- initSMGen
  print $ simulateGraphs
    [zipfWorkload 1.5 10000 10000 gen]
    [ LRU $ LRUList []
    , FIFO $ FIFOList []
    , LFU $ LFUHeap Heap.empty
    , DLFU $ DLFUHeap Heap.empty
    ]
    [1000, 2000, 4000]
  {-
  print $ simulateGraphs
    (  [ zipfWorkload alpha keyRange 10000 gen
       | alpha    <- [1.5, 1.6, 1.7, 1.8, 1.9, 2.0]
       , keyRange <- [2 ^ 13, 2 ^ 14, 2 ^ 15]
       ]
    ++ [ uniformWorkload keyRange 10000 gen
       | keyRange <- [2 ^ 13, 2 ^ 14, 2 ^ 15]
       ]
    )
    [ LRU $ LRUList []
    , FIFO $ FIFOList []
    , LFU $ LFUHeap Heap.empty
    , DLFU $ DLFUHeap Heap.empty
    ]
    [2 ^ 10, 2 ^ 11, 2 ^ 12, 2 ^ 13]
    -}
