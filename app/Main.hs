module Main where

import qualified Data.Heap                     as Heap
import qualified Data.List                     as List
import           Data.List.Split                ( splitOn )
import           Lib                            ( DLFU(..)
                                                , FIFO(..)
                                                , LFU(..)
                                                , LRU(..)
                                                , Wrapper(..)
                                                , arcTraceWorkload
                                                , arcTraceWorkload'
                                                , arcTraces
                                                , simulateGraphs
                                                , simulateGraphsSerially
                                                , uniformWorkload
                                                , zipfWorkload
                                                )
import           System.Environment             ( getArgs
                                                , getProgName
                                                )
import           System.Exit                    ( die )
import           System.Random.SplitMix         ( initSMGen )

main :: IO ()
main = do
  args <- getArgs
  usageOrRun args
 where
  usageOrRun [serialOrParallel, workloadSpec, cacheSizesSpec] = do
    let executionStrategy = resolveExecutionStrategy serialOrParallel
    let sizes = interpolateLogSteps $ map read $ splitOn "," cacheSizesSpec

    -- Workloads
    ds1  <- arcTraceWorkload "ds1.arc.gz"
    oltp <- arcTraceWorkload "oltp.arc.gz"
    p3   <- arcTraceWorkload "p3.arc.gz"
    p8   <- arcTraceWorkload "p8.arc.gz"
    s3   <- arcTraceWorkload "s3.arc.gz"

    let arc =
          [ ("ARC DS1" , take 50000 ds1)
          , ("ARC OLTP", take 50000 oltp)
          , ("ARC P3"  , take 50000 p3)
          , ("ARC P8"  , take 50000 p8)
          , ("ARC S3"  , take 50000 s3)
          ]

    gen <- initSMGen
    let skewed =
          [ ( "Zipfian " ++ show alpha ++ ", " ++ show keyRange
            , zipfWorkload alpha keyRange 10000 gen
            )
          | alpha    <- [1.1, 1.3, 1.5]
          , keyRange <- [2 ^ 8, 2 ^ 9, 2 ^ 10]
          ]
    let uniform = [("Uniform 1024", uniformWorkload (2 ^ 10) 10000 gen)]

    let workload =
          buildWorkload [("skewed", skewed), ("uniform", uniform), ("arc", arc)]
            $ splitOn "," workloadSpec

    -- Run simulation
    print sizes
    print $ executionStrategy
      workload
      [ LRU $ LRUList []
      , FIFO $ FIFOList []
      , LFU $ LFUHeap Heap.empty
      , DLFU $ DLFUHeap Heap.empty
      ]
      sizes
  usageOrRun _ = do
    pname <- getProgName
    die $ "Usage: " ++ pname ++ " serial|parallel 'uniform,skewed,arc' '6,7,8'"
  interpolateLogSteps sizes = concatMap
    (\size -> [ 2 ^ size + (2 ^ (size - 2)) * step | step <- [0 .. 3] ])
    sizes
  buildWorkload
    :: [(String, [(String, [Int])])] -> [String] -> [(String, [Int])]
  buildWorkload _ [] = []
  buildWorkload workloads (w : ws) =
    case List.find (\w' -> fst w' == w) workloads of
      Just (_, workload) -> workload ++ buildWorkload workloads ws
      Nothing            -> buildWorkload workloads ws
  resolveExecutionStrategy "serial"   = simulateGraphsSerially
  resolveExecutionStrategy "parallel" = simulateGraphs
  resolveExecutionStrategy _ =
    error "Valid execution strategies: serial, parallel"
