module Workloads
  ( cycleWorkload
  , uniformWorkload
  , zipfWorkload
  , arcTraceWorkload
  , arcTraces
  , histogram
  , arcTraceWorkload'
  ) where

import           Codec.Compression.GZip         ( decompress )
import qualified Data.ByteString.Lazy          as ByteString
import qualified Data.ByteString.Lazy.UTF8     as UTF8
import           Data.List                      ( group )
import           Data.Sort                      ( sort )
import           System.Random.SplitMix         ( SMGen
                                                , nextWord64
                                                )
import           System.Random.SplitMix.Distributions
                                                ( sample
                                                , samples
                                                , uniformR
                                                , zipf
                                                )

cycleWorkload :: (Num a, Enum a) => a -> Int -> [a]
cycleWorkload keyRange numOps = take numOps $ cycle [1 .. keyRange]

uniformWorkload :: Integral b => Int -> Int -> SMGen -> [b]
uniformWorkload keyRange numOps gen = map round
  $ samples numOps (fst $ nextWord64 gen) (uniformR 1 (fromIntegral keyRange))

zipfWorkload :: (Eq t, Num t, Integral a) => Double -> a -> t -> SMGen -> [a]
zipfWorkload _     _        0      _   = []
zipfWorkload alpha keyRange numOps gen = fst nextValue
  : zipfWorkload alpha keyRange (numOps - 1) (snd nextValue)
 where
  nextValue = genNextValue gen
  genNextValue gen' = if fst candidate <= keyRange
    then candidate
    else genNextValue (snd nextSeed)
   where
    candidate = (sample (fst nextSeed) (zipf alpha), snd nextSeed)
    nextSeed  = nextWord64 gen'

histogram :: Ord a => [a] -> IO ()
histogram l = mapM_ (putStrLn . encoded) runs
 where
  runs = (group . sort) l
  encoded r = replicate (length r) '*'

arcTraces :: [[Char]]
arcTraces =
  ["ds1.arc.gz", "oltp.arc.gz", "s3.arc.gz", "p3.arc.gz", "p8.arc.gz"]

arcTraceWorkload :: (Num b, Read b, Enum b) => [Char] -> IO [b]
arcTraceWorkload traceFile = do
  traceContent <- fmap (UTF8.toString . decompress)
                       (ByteString.readFile ("traces/arc/" ++ traceFile))
  return (concatMap traceAccesses $ lines traceContent)
 where
  traceAccesses line = accessSequence $ words line
  accessSequence (base : count : _) = map (+ read base) [1 .. (read count)]
  accessSequence _                  = []


arcTraceWorkload' :: (Num b, Read b, Enum b) => [Char] -> IO [b]
arcTraceWorkload' traceFile = do
  contents <- readFile ("traces/arc/" ++ traceFile)
  return (concatMap traceAccesses $ lines contents)
 where
  traceAccesses line = accessSequence $ words line
  accessSequence (base : count : _) = map (+ read base) [1 .. (read count)]
  accessSequence _                  = []

