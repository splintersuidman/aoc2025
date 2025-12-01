module Aoc2025.Util where

import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Word (Word8)
import Numeric.Natural (Natural)
import Streamly.Data.Array (Array)
import Streamly.Data.Array qualified as Array
import Streamly.Data.Fold (Fold)
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Parser (Parser)
import Streamly.Data.Parser qualified as Parser
import Streamly.Data.Stream (Stream)
import Streamly.Data.Stream qualified as Stream
import Streamly.Internal.Data.Parser qualified as Parser

showText :: Show a => a -> Text
showText = Text.pack . show
{-# INLINE showText #-}

showByteStringUtf8 :: Show a => a -> ByteString
showByteStringUtf8 = Text.encodeUtf8 . showText
{-# INLINE showByteStringUtf8 #-}

castEnum :: (Enum a, Enum b) => a -> b
castEnum = toEnum . fromEnum
{-# INLINE castEnum #-}

-- | Parser for decimal natural numbers.
naturalParser :: (Monad m) => Parser Word8 m Natural
naturalParser = Parser.takeWhile1 isDigit $ Fold.foldl' (\acc d -> 10 * acc + fromDigit d) 0
  where
    zero, nine :: Word8
    zero = castEnum '0'
    nine = castEnum '9'
    isDigit c = zero <= c && c <= nine
    fromDigit c = castEnum $ c - zero

-- | Parser for non-negative decimal integers.
nonNegIntegerParser :: (Monad m) => Parser Word8 m Integer
nonNegIntegerParser = toInteger <$> naturalParser

-- | Parser for decimal integers.
integerParser :: (Monad m) => Parser Word8 m Integer
integerParser
  =  negate <$> (Parser.satisfy (== castEnum '-') *> nonNegIntegerParser)
 <|> nonNegIntegerParser

parseSepBy :: (Monad m) => (a -> Bool) -> Parser a m b -> Stream m a -> Stream m (Either Parser.ParseError b)
parseSepBy pred p = Stream.parseMany $ p <* sep
 where
  sep = Parser.takeWhile1 pred Fold.drain <|> Parser.fromPure ()

char :: (Eq a, Monad m) => a -> Parser a m a
char c = Parser.satisfy (== c)

debug :: (Monad m) => (a -> m b) -> Stream m a -> Stream m a
debug f = Stream.tap (Fold.drainMapM f)
