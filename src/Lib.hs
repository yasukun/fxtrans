{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( quote
    , csvFile
    , quoteParser
    , quoteParser2
    , groupQuote
    , mergeQuote
    ) where

import Control.Applicative
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import Data.Time
import Data.List
import Data.Text (Text, unpack, pack)
import qualified Data.Text as T
import Data.Maybe
import Text.Printf

data Quote = Quote {
      qTime :: LocalTime,
      qOpen :: Double,
      qHigh :: Double,
      qLow :: Double,
      qClose :: Double,
      qVolume :: Double
    } deriving (Show, Eq)


csvFile :: Parser [Quote]
csvFile = do
    q <- many1 quote
    endOfInput
    return q

-- csvFile :: Parser [Quote]
-- csvFile = many1 quote <* endOfLine

qcomma  :: Parser ()
qcomma = char ',' *> pure ()

qtime   :: Parser LocalTime
qtime = createTime <$> takeTill (\x -> x == ',')
        where defaultTime = LocalTime (fromGregorian 0001 01 01) (TimeOfDay 00 00 00)
              parseTimeText t = parseTimeM False defaultTimeLocale "%Y/%m/%d %H:%M" (unpack $ roundTime 5 t)
              createTime x = fromMaybe defaultTime $ parseTimeText x

quote :: Parser Quote
quote = Quote <$> (qtime <* qcomma)
              <*> (double <* qcomma)
              <*> (double <* qcomma)
              <*> (double <* qcomma)
              <*> (double <* qcomma)
              <*> (double <* endOfLine)

testString :: Text
testString = "2015/12/30 00:01,1.092575,1.0927,1.09255,1.09261,336\n"

testString2 :: Text
testString2="2015/12/30 00:01,1.092575,1.0927,1.09255,1.09261,336\n2015/12/30 00:02,1.09261,1.0928,1.09258,1.092675,423\n"

quoteParser = parseOnly quote

quoteParser2 = parseOnly csvFile

roundTick :: Integral a => a -> a -> a
roundTick tick 60 = 0
roundTick tick sec= tick + ((sec `div` tick) - 1) * tick

roundTime :: Int -> Text -> Text
roundTime tick timetxt =
    T.append forward . pack $ printf "%02d" min
    where
      (forward, back) = T.splitAt 14 timetxt
      min = roundTick tick . read $ unpack back :: Int

groupQuote :: [Quote] -> [[Quote]]
groupQuote = groupBy (\x y -> let (Quote {qTime = qa}) = x
                                  (Quote {qTime = qb}) = y
                              in qa ==  qb)

mergeQuote qs = foldl (\x y -> let (Quote {  qVolume = qv
                                           , qHigh = qh
                                           , qLow = ql}) = y
                                   (Quote {  qVolume = xv
                                           , qHigh = xh
                                           , qLow = xl}) = x
                               in x {  qVolume = qv + xv
                                     , qHigh = max qh xh
                                     , qLow = min ql xl})
                Quote {qTime = lTime, qOpen = lOpen, qClose = lClose, qVolume = 0, qHigh = 0, qLow = 1/0} qs
        where (Quote {qTime = lTime, qOpen = lOpen}) = head qs
              (Quote {qClose = lClose}) = last qs
