{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
module Main where

{- ORMOLU_DISABLE -}
--- Year imports
import Years (yearDaysMap)

--- Other imports
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Options.Applicative
import qualified Control.Applicative.Combinators as C (option)
import Program.RunDay (Day, Verbosity (Quiet, Timings, Verbose))
import Data.List (intercalate)
import Control.Monad (unless, forM_)

-- Data Output
import Text.Printf (printf)
import Program.Color ( withColor )
import System.Console.ANSI (Color(..))
{- ORMOLU_ENABLE -}

data Days
  = AllDays
  | OneDay
      { day :: DayNumber,
        input :: Maybe String,
        exampleInput :: Bool
      }
  deriving (Show)

type Year = Int
type DayNumber = Int

type InputPath = String

data Options = Options Year Days Verbosity

yearParser :: Parser Year
yearParser = 
  option auto $
        long "year" <> short 'y' <> metavar "YEAR"
          <> help "Select the year."

dayParser :: Parser Days
dayParser = (OneDay <$> day <*> input <*> exampleFlag) <|> allDays
  where
    day =
      option auto $
        long "day" <> short 'd' <> metavar "DAY"
          <> help "Present the solutions for one day."

    input =
      optional $
        strOption $
          long "input" <> short 'i' <> metavar "FILE"
            <> help "The file to read the selected day's input from."

    exampleFlag =
      flag False True $
        long "exampleInput" <> short 'e' 
          <> help "Select the example input (expected name: DayXXExample.txt)"

    allDays =
      flag' AllDays $
        long "all-days"
          <> help
            ( unwords
                [ "Present solutions for all of the days of",
                  "Advent of Code, with default input file names."
                ]
            )

optionsParser :: Parser Options
optionsParser = Options <$> yearParser <*> dayParser <*> verbosityParser
  where
    verbosityParser :: Parser Verbosity
    verbosityParser =
      C.option Quiet $
        ( flag' Verbose $
            long "verbose" <> short 'v'
              <> help
                ( unwords
                    [ "Whether to print out extra info, such as the",
                      "result of the input parser, and more detailed",
                      "error messages.",
                      "Also enables timing of solutions."
                    ]
                )
        )
          <|> ( flag' Timings $
                  long "timings" <> short 't'
                    <> help
                      ( unwords
                          ["Whether to enable timing of the solutions."]
                      )
              )

performDay :: Options -> IO ()
performDay (Options year day verbosity) = case day of
  AllDays -> do
    results <-
      let eachDay day dayFunc = do
            withColor Magenta $ putStrLn $ printf "\n***Day %02d***" day
            dayFunc verbosity $ getInputFilePath year day False
       in sequence $ Map.mapWithKey eachDay $ yearDaysMap Map.! year

    printSummary results
  OneDay {..} -> case yearDaysMap Map.!? year >>= \days -> days Map.!? day of
    Nothing -> putStrLn $ "Invalid year and day provided for " ++ show year ++ "-12-" ++ show day 
    Just dayFunc -> do
      let selectedInputFilePath = fromMaybe (getInputFilePath year day exampleInput) input
      withColor Magenta $ putStrLn $ printf "\n***Day %02d***" day
      dayFunc verbosity selectedInputFilePath
      withColor Magenta $ putStrLn "************"


-- >>> getInputFilePath 2022 9 False
-- "input/2022/Day09.txt"
-- >>> getInputFilePath 2022 9 True
-- "input/2022/Day09Example.txt"
getInputFilePath :: Year -> DayNumber -> Bool -> InputPath
getInputFilePath year day isExample = printf "input/%04d/Day%02d%s.txt" year day exampleString
  where
    exampleString = 
      if isExample
        then
          "Example" :: String
        else
          ""


printSummary :: Map Int (Maybe Double, Maybe Double) -> IO ()
printSummary results = do
  putStrLn "\n************\n  Summary:  "
  let partsA = Map.mapKeys ((++ " (a)") . printf "%02d") $ fmap fst results
      partsB = Map.mapKeys ((++ " (b)") . printf "%02d") $ fmap snd results
      parts = Map.toList $ partsA <> partsB

      fails = [p | (p, Nothing) <- parts]
      fasts = [(p, t) | (p, Just t) <- parts, t < 1]
      slows = [(p, t) | (p, Just t) <- parts, t >= 1]

  putStr $ printf "\n%d parts " $ length fasts
  withColor Green $ putStr "completed in under 1 second"
  putStrLn ".\nOf the remainder:"
  unless (null fails) $ do
    putStr $ printf "  %d parts" $ length fails
    withColor Red $ putStr " failed"
    putStrLn $ ":\n    " ++ intercalate ", " fails
  unless (null slows) $ do
    putStr $ printf "  %d parts" $ length slows
    withColor Yellow $ putStr " took over 1 second to complete"
    putStrLn ":"
    forM_ slows $
      \(p, t) -> putStrLn $ printf "    %s took %.2f seconds" p t

main :: IO ()
main = performDay =<< execParser opts
  where
    opts =
      info
        (optionsParser <**> helper)
        (fullDesc <> progDesc "Prints out some Advent of Code solutions.")
