module Main where

import System.Environment
import Data.Text (pack)
import Lib
import Control.Monad

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> print "done"
    _  -> forM_ args (\x -> do
                        csv <- readFile x
                        let (Right result) = quoteParser2 $ pack csv
                        print $ map (\x -> mergeQuote x) $ groupQuote result)
