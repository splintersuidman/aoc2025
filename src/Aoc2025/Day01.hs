{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Aoc2025.Day01 where

import Aoc2025.Day
import Aoc2025.Util (castEnum, char, naturalParser, parseSepBy, showByteStringUtf8)
import Control.Applicative ((<|>))
import Control.Exception (throwIO)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Modular (type (/))
import Data.Modular qualified as Mod
import Data.Word (Word8)
import Numeric.Natural (Natural)
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Parser (Parser)
import Streamly.Data.Stream qualified as Stream

day :: Day
day = Day (Just part1) (Just part2)

type Rotation = Either Natural Natural

rotationParser :: (Monad m) => Parser Word8 m Rotation
rotationParser =
  Left <$> (char (castEnum 'L') *> naturalParser)
    <|> Right <$> (char (castEnum 'R') *> naturalParser)

type Dial = Integer / 100

startDial :: Dial
startDial = 50

rotate :: Dial -> Rotation -> Dial
rotate dial = \case
  Left rotations -> dial - Mod.toMod' rotations
  Right rotations -> dial + Mod.toMod' rotations

part1 :: Part
part1 = Part $ \source ->
  sourceToStream source
    & parseSepBy (== castEnum '\n') rotationParser
    & Stream.mapM (either throwIO pure)
    & Stream.scan (Fold.foldl' rotate startDial)
    & Stream.filter (== 0)
    & Stream.fold Fold.length
    <&> showByteStringUtf8

data Dial' = Dial'
  { clicks :: Natural,
    position :: Integer
  }
  deriving (Eq, Show)

startDial' :: Dial'
startDial' = Dial' {clicks = 0, position = 50}

rotate' :: Dial' -> Rotation -> Dial'
rotate' (Dial' clicks position) rotation =
  let fictitiousNewPosition =
        case rotation of
          Left r -> position - toInteger r
          Right r -> position + toInteger r
      newClicks =
        clicks
          + fromInteger (abs (fictitiousNewPosition `quot` 100))
          + if fictitiousNewPosition <= 0 && position /= 0 then 1 else 0
      newPosition = fictitiousNewPosition `mod` 100
   in Dial' newClicks newPosition

part2 :: Part
part2 = Part $ \source ->
  sourceToStream source
    & parseSepBy (== castEnum '\n') rotationParser
    & Stream.mapM (either throwIO pure)
    & Stream.fold (Fold.foldl' rotate' startDial')
    <&> showByteStringUtf8 . clicks
