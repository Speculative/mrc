module Main where

import qualified Data.Heap                     as Heap
import           Debug.Trace                    ( trace )
import           Lib                            ( DLFU(..)
                                                , FIFO(..)
                                                , LFU(..)
                                                , LRU(..)
                                                , Wrapper(..)
                                                , arcTraceWorkload
                                                , arcTraceWorkload'
                                                , arcTraces
                                                , simulateGraphs
                                                , uniformWorkload
                                                , zipfWorkload
                                                )
import           System.Random.SplitMix         ( initSMGen )

main :: IO ()
main = do
  gen <- initSMGen
  -- let sizes = interpolateLogSteps [6, 7, 8]
  let sizes = [2 ^ 12, 2 ^ 14, 2 ^ 15, 2 ^ 16]
  print sizes
  {-
  print $ simulateGraphs
    (  [ ( "Zipfian " ++ show alpha ++ ", " ++ show keyRange
         , zipfWorkload alpha keyRange 10000 gen
         )
       | alpha    <- [1.1, 1.3, 1.5]
       , keyRange <- [2 ^ 8, 2 ^ 9, 2 ^ 10]
       ]
    ++ [("Uniform 1024", uniformWorkload (2 ^ 10) 10000 gen)]
    )
    [ LRU $ LRUList []
    , FIFO $ FIFOList []
    , LFU $ LFUHeap Heap.empty
    , DLFU $ DLFUHeap Heap.empty
    ]
    sizes
  -}
  -- ds1 <- arcTraceWorkload' "oltp.arc"
  oltp <- arcTraceWorkload' "oltp.arc"
  -- p3   <- arcTraceWorkload "p3.arc.gz"
  -- p8   <- arcTraceWorkload "p8.arc.gz"
  -- s3   <- arcTraceWorkload "s3.arc.gz"

  print $ simulateGraphs
    [
      -- ("ARC DS1", take 80000 ds1)
     ("ARC OLTP", take 70000 oltp)
    -- , ("ARC P3"  , take 100000 p3)
    -- , ("ARC P8"  , take 100000 p8)
    -- , ("ARC S3"  , take 100000 s3)
                                  ]
    [ LRU $ LRUList []
    , FIFO $ FIFOList []
    , LFU $ LFUHeap Heap.empty
    , DLFU $ DLFUHeap Heap.empty
    ]
    sizes
 where
  interpolateLogSteps sizes = concatMap
    (\size -> [ 2 ^ size + (2 ^ (size - 2)) * step | step <- [0 .. 3] ])
    sizes
