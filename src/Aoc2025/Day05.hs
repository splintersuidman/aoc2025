{-# LANGUAGE LambdaCase #-}

module Aoc2025.Day05 where

import Aoc2025.Day
import Aoc2025.Util (castEnum, parseSepBy, showByteStringUtf8, naturalParser)
import Control.Applicative ((<|>))
import Control.Exception (throwIO)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Word (Word8)
import Numeric.Natural (Natural)
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Parser (Parser)
import Streamly.Data.Parser qualified as Parser
import Streamly.Data.Stream qualified as Stream
import Streamly.Internal.Data.Fold qualified as Fold

day :: Day
day = Day (Just part1) (Just part2)

data Range = Range {rangeMin :: Natural, rangeMax :: Natural}
  deriving (Eq)

instance Show Range where
  show (Range a b) = show a <> "-" <> show b

rangeParser :: (Monad m) => Parser Word8 m Range
rangeParser = Range <$> naturalParser <*> (Parser.satisfy (== castEnum '-') *> naturalParser)

rangeSize :: Range -> Natural
rangeSize (Range a b) = b - a + 1

newtype RangeSet = RangeSet (Map Natural Natural)
  deriving (Eq)

rangeSetEmpty :: RangeSet
rangeSetEmpty = RangeSet Map.empty

rangeSetToList :: RangeSet -> [Range]
rangeSetToList (RangeSet s) = fmap (uncurry Range) $ Map.toAscList s

rangeSetElem :: Natural -> RangeSet -> Maybe Range
rangeSetElem n (RangeSet s) = case Map.lookupLE n s of
  Just (a, b) | n <= b -> Just (Range a b)
  _ -> Nothing

instance Show RangeSet where
  show s = "(" <> List.intercalate ", " (fmap show $ rangeSetToList s) <> ")"

rangeSetCons :: Range -> RangeSet -> RangeSet
rangeSetCons (Range a b) (RangeSet s) = RangeSet $ case submerge <|> mergeBoth <|> mergeLeft <|> mergeRight of
  Just s' -> s'
  Nothing -> Map.insert a b s
  where
    submerge = case Map.lookupLE a s of
      Just (_c, d) | b <= d -> Just s
      _ -> Nothing
    mergeBoth = case (Map.lookupLE a s, Map.lookupGT a s) of
      (Just (c, d), Just (e, f)) | a <= d + 1, e <= b + 1 -> Just $ Map.insert c f $ Map.delete e $ Map.delete c s
      _ -> Nothing
    mergeLeft = case Map.lookupLE a s of
      Just (c, d) | a <= d + 1 -> Just $ Map.insert c (max b d) s
      _ -> Nothing
    mergeRight = case Map.lookupGE a s of
      Just (c, d) | c <= b + 1 -> Just $ Map.insert a (max b d) $ Map.delete c s
      _ -> Nothing

data Input = InputRange Range | InputQuery Natural
  deriving (Eq)

instance Show Input where
  show = \case
    InputRange r -> show r
    InputQuery n -> show n

inputParser :: (Monad m) => Parser Word8 m Input
inputParser = InputRange <$> rangeParser <|> InputQuery <$> naturalParser

inputToEither :: Input -> Either Range Natural
inputToEither = \case
  InputRange r -> Left r
  InputQuery n -> Right n

part1 :: Part
part1 = Part $ \source -> do
  (rangeSet, queries) <- sourceToStream source
    & parseSepBy (== castEnum '\n') inputParser
    & Stream.mapM (either throwIO pure)
    & Stream.fold (Fold.partitionBy inputToEither (Fold.foldl' (flip rangeSetCons) rangeSetEmpty) Fold.toStream)
  queries
    & Stream.filter (isJust . (`rangeSetElem` rangeSet))
    & Stream.fold Fold.length
    <&> showByteStringUtf8

part2 :: Part
part2 = Part $ \source -> do
  rangeSet <- sourceToStream source
    & parseSepBy (== castEnum '\n') inputParser
    & Stream.mapM (either throwIO pure)
    & fmap inputToEither
    & Stream.catLefts
    & Stream.fold (Fold.foldl' (flip rangeSetCons) rangeSetEmpty)
  pure $ showByteStringUtf8 $ sum $ fmap rangeSize $ rangeSetToList rangeSet
