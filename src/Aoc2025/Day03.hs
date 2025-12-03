{-# LANGUAGE DeriveAnyClass #-}

module Aoc2025.Day03 where

import Aoc2025.Day
import Aoc2025.Util (castEnum, castIntegral, parseSepBy, showByteStringUtf8)
import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (MonadIO)
import Data.Char (isDigit)
import Data.Function (on, (&))
import Data.Functor ((<&>))
import Data.Functor.Identity (runIdentity)
import Data.Word (Word8)
import Numeric.Natural (Natural)
import Streamly.Data.Array (Array)
import Streamly.Data.Array qualified as Array
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Parser (Parser)
import Streamly.Data.Parser qualified as Parser
import Streamly.Data.Stream qualified as Stream

day :: Day
day = Day (Just part1) (Just part2)

type Battery = Int

type Bank = Array Battery

findBiggestLeadingIndex :: Bank -> Maybe Int
findBiggestLeadingIndex bank =
  Array.read bank
    & Stream.indexed
    & Stream.take (Array.length bank - 1)
    & Stream.reverse -- because Fold.maximumBy will find the last biggest
    & Stream.fold (Fold.maximumBy (compare `on` snd))
    & runIdentity
    <&> fst

findBiggestFinalIndex :: Bank -> Int -> Maybe Int
findBiggestFinalIndex bank leadingIndex =
  Array.read bank
    & Stream.indexed
    & Stream.drop (leadingIndex + 1)
    & Stream.fold (Fold.maximumBy (compare `on` snd))
    & runIdentity
    <&> fst

bankJoltage :: Bank -> Maybe Int
bankJoltage bank = do
  leadingIndex <- findBiggestLeadingIndex bank
  finalIndex <- findBiggestFinalIndex bank leadingIndex
  leadingDigit <- Array.getIndex leadingIndex bank
  finalDigit <- Array.getIndex finalIndex bank
  pure $ 10 * leadingDigit + finalDigit

batteryParser :: (Monad m) => Parser Word8 m Battery
batteryParser = (\x -> castEnum x - castEnum '0') <$> Parser.satisfy (isDigit . castEnum)

bankParser :: (Monad m, MonadIO m) => Parser Word8 m Bank
bankParser = Parser.many batteryParser Array.write

data BankNotBigEnoughException = BankNotBigEnoughException deriving (Show, Exception)

part1 :: Part
part1 = partForLength 2

findBiggestLeadingIndexForLengthAfter :: Bank -> Int -> Maybe Int -> Maybe Int
findBiggestLeadingIndexForLengthAfter bank len after =
  Array.read bank
    & Stream.indexed
    & Stream.drop dropAmount
    & Stream.take (Array.length bank - dropAmount - len + 1)
    & Stream.reverse -- because Fold.maximumBy will find the last biggest
    & Stream.fold (Fold.maximumBy (compare `on` snd))
    & runIdentity
    <&> fst
  where
    dropAmount = maybe 0 (+ 1) after

bankJoltageForLength :: Int -> Bank -> Maybe Natural
bankJoltageForLength bankLength bank = go bankLength Nothing
  where
    go 0 _ = Just 0
    go len after = do
      leadingIndex <- findBiggestLeadingIndexForLengthAfter bank len after
      rest <- go (len - 1) (Just leadingIndex)
      leadingDigit <- Array.getIndex leadingIndex bank
      pure $ 10 ^ (len - 1) * castIntegral leadingDigit + rest

part2 :: Part
part2 = partForLength 12

partForLength :: Int -> Part
partForLength len = Part $ \source ->
  sourceToStream source
    & parseSepBy (== castEnum '\n') bankParser
    & Stream.mapM (either throwIO pure)
    & fmap (bankJoltageForLength len)
    & Stream.mapM (maybe (throwIO BankNotBigEnoughException) pure)
    & Stream.fold Fold.sum
    <&> showByteStringUtf8
