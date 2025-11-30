{-# LANGUAGE DataKinds #-}

module Aoc2025.App.Args where

import Options.Applicative qualified as Options

parseArgs :: IO Command
parseArgs = Options.execParser opts

opts :: Options.ParserInfo Command
opts = Options.info commandParser
  ( Options.fullDesc
 <> Options.progDesc "Run and test solutions for Advent of Code 2025"
 <> Options.header "aoc2025 – solutions for Advent of Code 2025"
  )

data Parts
  = PartOne
  | PartTwo
  | BothParts

data TestInput
  = TestFiles
  | TestStdin

data Command where
  CommandRun :: Int -> Parts -> Command
  CommandTest :: Int -> Parts -> TestInput -> Command

commandParser :: Options.Parser Command
commandParser = Options.subparser
  ( Options.command "run"
    (Options.info runCommandParser $ Options.progDesc "Run on input")
 <> Options.command "test"
    (Options.info testCommandParser $ Options.progDesc "Test")
  )

runCommandParser :: Options.Parser Command
runCommandParser = CommandRun
  <$> Options.argument dayReader (Options.metavar "DAY")
  <*> Options.argument partsReader (Options.metavar "PARTS")

testCommandParser :: Options.Parser Command
testCommandParser = CommandTest
  <$> Options.argument dayReader (Options.metavar "DAY")
  <*> Options.argument partsReader (Options.metavar "PARTS")
  <*> Options.flag TestFiles TestStdin (Options.long "stdin" <> Options.help "Use input from stdin")

partsReader :: Options.ReadM Parts
partsReader = Options.eitherReader $ \str -> case str of
  "1" -> pure PartOne
  "2" -> pure PartTwo
  "both" -> pure BothParts
  "all" -> pure BothParts
  _ -> Left $ "Invalid part ‘" <> str <> "’"

dayReader :: Options.ReadM Int
dayReader = Options.eitherReader $ \str -> case reads str of
  [(d, "")] | 1 <= d && d <= 12 -> pure d
  _ -> Left $ "Invalid day ‘" <> str <> "’, must be integer between 1 and 12"
