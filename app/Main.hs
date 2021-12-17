module Main where

import           Lib
import           Workloads                      ( arcTraceWorkload
                                                , arcTraces
                                                )

main :: IO ()
main = do
  workloads <- mapM arcTraceWorkload arcTraces
  print $ map length workloads
