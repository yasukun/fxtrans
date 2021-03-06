module Main where

import System.Environment
import Data.Text (pack)
import Lib
import Control.Monad
import Data.List (nub)
import Algo.Lib

-- main :: IO ()
-- main = do
--   args <- getArgs
--   case args of
--     [] -> print "done"
--     _  -> forM_ args (\x -> do
--                         csv <- readFile x
--                         let (Right result) = quoteParser2 $ pack csv
--                         print $ map (\x -> mergeQuote x) $ groupQuote result)


main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> print "done"
    _ -> forM_ args (\x -> do
                       csv <- readFile x
                       let (Right result) = quoteParser2 $ pack csv
                           result2 = map (\x -> mergeQuote x) $ groupQuote result
                           result3 = tstoargs2 $ indexedret result2
                       -- print $ kleinberg (tstoargs result) defOpts
                       print $ map snd result3
                       print $ kleinberg (map fst result3) defOpts
                    )
