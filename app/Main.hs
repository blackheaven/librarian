module Main (main) where

import Convert (convert)
import qualified Dhall as Dhall
import DhallTypes as DhallTypes
import Librarian
import Options.Applicative as Options

main :: IO ()
main = do
  args <- parseArgs
  rules <- map convert <$> Dhall.inputFile (Dhall.auto @[DhallTypes.Rule]) (rulesFile args)
  plan <- planActions <$> fetchRulesOn "." rules
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
