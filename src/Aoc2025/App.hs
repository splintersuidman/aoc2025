{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}

module Aoc2025.App where

import Aoc2025.App.Args (Command (..), Parts (..), TestInput (..), parseArgs)
import Aoc2025.Day
import Aoc2025.Day01 qualified as Day01
import Aoc2025.Day02 qualified as Day02
import Aoc2025.Day03 qualified as Day03
import Aoc2025.Day04 qualified as Day04
import Aoc2025.Day05 qualified as Day05
import Aoc2025.Day06 qualified as Day06
import Aoc2025.Day07 qualified as Day07
import Aoc2025.Day08 qualified as Day08
import Aoc2025.Day09 qualified as Day09
import Aoc2025.Day10 qualified as Day10
import Aoc2025.Day11 qualified as Day11
import Aoc2025.Day12 qualified as Day12
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteString
import Data.Foldable (for_)
import Data.List (intercalate, isPrefixOf)
import Data.Set qualified as Set
import System.Directory qualified as Directory
import System.IO (IOMode (ReadMode), hPutStr, hPutStrLn, openFile, stderr)

days :: [Day]
days =
  [ Day01.day,
    Day02.day,
    Day03.day,
    Day04.day,
    Day05.day,
    Day06.day,
    Day07.day,
    Day08.day,
    Day09.day,
    Day10.day,
    Day11.day,
    Day12.day
  ]

listParts :: Day -> Parts -> [(Int, Maybe Part)]
listParts day = \case
  PartOne -> [part1]
  PartTwo -> [part2]
  BothParts -> [part1, part2]
  where
    part1 = (1, dayPart1 day)
    part2 = (2, dayPart2 day)

app :: IO ()
app = do
  parseArgs >>= \case
    CommandRun d parts -> do
      let day = days !! (d - 1)
      let source = SourceFile $ "input/day/" <> show d <> "/input"
      for_ (listParts day parts) $ \(p, mPart) -> do
        hPutStrLn stderr $ "-----------------------------"
        hPutStrLn stderr $ "        DAY " <> show d <> ", PART " <> show p
        hPutStrLn stderr $ "-----------------------------"

        case mPart of
          Nothing -> hPutStrLn stderr "Not yet implemented"
          Just part -> do
            output <- runPart part source
            ByteString.putStrLn output

        hPutStrLn stderr ""
    CommandTest d parts TestStdin -> do
      let day = days !! (d - 1)
      for_ (listParts day parts) $ \(p, mPart) -> do
        hPutStrLn stderr $ "-----------------------------"
        hPutStrLn stderr $ "        DAY " <> show d <> ", PART " <> show p
        hPutStrLn stderr $ "-----------------------------"

        case mPart of
          Nothing -> hPutStrLn stderr "Not yet implemented"
          Just part -> do
            -- Reopen stdin in case both parts are tested.
            stdin <- openFile "/dev/tty" ReadMode
            hPutStrLn stderr "Enter input (terminate with EOF):"
            input <- ByteString.hGetContents stdin
            let source = SourceLiteral input
            output <- runPart part source
            hPutStrLn stderr "Output:"
            ByteString.putStrLn output

        hPutStrLn stderr ""
    CommandTest d parts TestFiles -> do
      let day = days !! (d - 1)
      for_ (listParts day parts) $ \(p, mPart) -> do
        hPutStrLn stderr $ "-----------------------------"
        hPutStrLn stderr $ "        DAY " <> show d <> ", PART " <> show p
        hPutStrLn stderr $ "-----------------------------"

        case mPart of
          Nothing -> hPutStrLn stderr "Not yet implemented"
          Just part -> do
            let testFileDirectory = "input/day/" <> show d <> "/test/part" <> show p <> "/"
            files <- Directory.listDirectory testFileDirectory
            let inputFiles = filter ("input" `isPrefixOf`) files
            let testFiles = flip fmap inputFiles $ \inputFile ->
                  let testName = drop (length "input") inputFile
                      outputFile = "output" <> testName
                   in ( testName,
                        inputFile,
                        if outputFile `elem` Set.fromList files
                          then Just outputFile
                          else Nothing
                      )

            hPutStrLn stderr $ "Testing " <> (intercalate ", " $ fmap (\(x, _, _) -> x) testFiles)
            for_ testFiles $ \(testName, inputFile, outputFile) -> do
              let source = SourceFile $ testFileDirectory <> inputFile
              output <- runPart part source
              case outputFile of
                Nothing -> do
                  hPutStr stderr $ "[ ] test case " <> testName <> " output: "
                  ByteString.putStrLn output
                Just filename -> do
                  expected <- ByteString.strip <$> ByteString.readFile (testFileDirectory <> filename)
                  if output == expected
                    then hPutStrLn stderr $ "[✅] test case " <> testName <> ": output correct"
                    else do
                      hPutStrLn stderr $ "[❌] test case " <> testName <> ": output incorrect:"
                      hPutStr stderr "  got: "
                      ByteString.hPutStrLn stderr output
                      hPutStr stderr "  expected: "
                      ByteString.hPutStrLn stderr expected

        hPutStrLn stderr ""
