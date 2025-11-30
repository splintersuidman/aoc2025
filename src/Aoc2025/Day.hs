{-# LANGUAGE LambdaCase #-}

module Aoc2025.Day where

import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Kind (Type)
import Data.Word (Word8)
import Streamly.Data.Array qualified as Array
import Streamly.Data.Stream (Stream)
import Streamly.External.ByteString qualified as ByteString
import Streamly.FileSystem.File qualified as File

data Source (m :: Type -> Type) where
  SourceLiteral :: ByteString -> Source m
  SourceFile :: (MonadIO m, MonadCatch m) => FilePath -> Source m

sourceToStream :: (Monad m) => Source m -> Stream m Word8
sourceToStream = \case
  SourceLiteral str -> Array.read $ ByteString.toArray str
  SourceFile filename -> File.read filename

sourceToByteString :: (Applicative m) => Source m -> m ByteString
sourceToByteString = \case
  SourceLiteral str -> pure str
  SourceFile filename -> liftIO $ ByteString.readFile filename

newtype Part = Part {runPart :: Source IO -> IO ByteString}

data Day = Day
  { dayPart1 :: Maybe Part,
    dayPart2 :: Maybe Part
  }
