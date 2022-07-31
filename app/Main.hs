module Main where

import qualified Dhall as Dhall
import Librarian
import Options.Applicative as Options

main :: IO ()
main = do
  args <- parseArgs
  rules <- Dhall.inputFile (Dhall.auto @[Rule]) $ rulesFile args
  plan <- planMoves <$> fetchRulesOn "." rules
  displayPlan plan
  putStrLn "Move? (y/n)"
  response <- getChar
  case response of
    'y' -> do
      putStrLn "Proceeding"
      runPlan plan >>= displayResult
      putStrLn "Done."
    _ -> putStrLn "Aborting"

parseArgs :: IO Args
parseArgs =
  execParser $
    info (argsParser <**> helper) (fullDesc <> header "Librarian: moving files, your rules!")
  where
    argsParser :: Parser Args
    argsParser =
      Args
        <$> strOption (long "rules" <> short 'r' <> metavar "FILE_PATH" <> help "Rules file (.dhall)")

newtype Args = Args {rulesFile :: FilePath}
  deriving stock (Eq, Ord, Show)
