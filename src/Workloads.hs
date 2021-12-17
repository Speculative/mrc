module Workload
  ( cycleWorkload
  , uniformWorkload
  , zipfWorkload
  ) where

import           Codec.Compression.GZip         ( decompress )
import           Data.List                      ( group )
import           Data.Sort                      ( sort )
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

arcTraces =
  ["ds1.arc.gz", "oltp.arc.gz", "s3.arc.gz", "p3.arc.gz", "p8.arc.gz"]

arcTraceWorkload traceFile = do
  traceContent <- fmap decompress (readFile "traces/arc/" ++ traceFile)
  concatMap traceAccesses $ lines traceContent
  where traceAccesses line = words line
