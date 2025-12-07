{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

module Aoc2025.Day07 where

import Aoc2025.Day
import Aoc2025.Util (castEnum, char, parseSepBy, showByteStringUtf8)
import Control.Applicative ((<|>))
import Control.Arrow (second)
import Control.Exception (throwIO)
import Control.Monad (guard)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (isJust)
import Data.Monoid (Sum (Sum, getSum))
import Data.Word (Word8)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Streamly.Data.Array (Array, Unbox)
import Streamly.Data.Array qualified as Array
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Parser (Parser)
import Streamly.Data.Parser qualified as Parser
import Streamly.Data.Stream qualified as Stream
import Streamly.Internal.Data.Fold qualified as Fold
import System.Exit (die)

day :: Day
day = Day (Just part1) (Just part2)

data TachyonManifoldCell = CellEnter | CellEmpty | CellSplitter
  deriving (Eq, Generic, Unbox)

instance Show TachyonManifoldCell where
  show = \case
    CellEnter -> "S"
    CellEmpty -> "."
    CellSplitter -> "^"

tachyonManifoldCellParser :: (Monad m) => Parser Word8 m TachyonManifoldCell
tachyonManifoldCellParser =
  CellEnter <$ char (castEnum 'S')
    <|> CellEmpty <$ char (castEnum '.')
    <|> CellSplitter <$ char (castEnum '^')

type Beams = Array Int

split :: (Beams, Int) -> Array TachyonManifoldCell -> IO (Beams, Int)
split (beams, splits) line =
  Array.read line
    & Stream.indexed
    & fmap doSplit
    & Stream.fold (Fold.unzip Array.create Fold.sum)
    <&> second (+ splits)
  where
    doSplit (i, CellSplitter) =
      ( 0,
        if maybe False (> 0) (Array.getIndex i beams) then 1 else 0
      )
    doSplit (i, _) =
      let unSplittedBeam = Sum <$> Array.getIndex i beams
          splitLeft = do
            guard $ Array.getIndex (i - 1) line == Just CellSplitter
            Sum <$> Array.getIndex (i - 1) beams
          splitRight = do
            guard $ Array.getIndex (i + 1) line == Just CellSplitter
            Sum <$> Array.getIndex (i + 1) beams
       in (maybe 0 getSum $ unSplittedBeam <> splitLeft <> splitRight, 0)

part1 :: Part
part1 = Part $ \source -> do
  s <-
    sourceToStream source
      & parseSepBy (== castEnum '\n') (Parser.many tachyonManifoldCellParser Array.create)
      & Stream.mapM (either throwIO pure)
      & Stream.uncons

  (enterBeams, rest) <- case s of
    Nothing -> die "empty stream"
    Just (enterLine, rest) -> do
      enterBeams <-
        Array.read enterLine
          & fmap (\cell -> if cell == CellEnter then 1 else 0)
          & Stream.fold Array.create
      pure (enterBeams, rest)

  rest
    & Stream.fold (Fold.foldlM' split $ pure (enterBeams, 0))
    <&> snd
    <&> showByteStringUtf8

part2 :: Part
part2 = Part $ \source -> do
  s <-
    sourceToStream source
      & parseSepBy (== castEnum '\n') (Parser.many tachyonManifoldCellParser Array.create)
      & Stream.mapM (either throwIO pure)
      & Stream.uncons

  (enterBeams, rest) <- case s of
    Nothing -> die "empty stream"
    Just (enterLine, rest) -> do
      enterBeams <-
        Array.read enterLine
          & fmap (\cell -> if cell == CellEnter then 1 else 0)
          & Stream.fold Array.create
      pure (enterBeams, rest)

  beams <-
    rest
      & Stream.fold (Fold.foldlM' split $ pure (enterBeams, 0))
      <&> fst

  beams
    & Array.read @IO
    & Stream.fold Fold.sum
    <&> showByteStringUtf8
