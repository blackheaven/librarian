module Main (main) where

import Convert (convert)
import qualified Data.Either.Validation as V
import qualified Data.Text.IO as Text.IO
import qualified Dhall as Dhall
import qualified Dhall.Core
import DhallTypes as DhallTypes
import Librarian
import Options.Applicative as Options
import System.Directory (withCurrentDirectory)

main :: IO ()
main = do
  args <- parseArgs
  case args of
    Format ->
      case Dhall.expected (Dhall.auto @[DhallTypes.Rule]) of
        V.Success result -> Text.IO.putStrLn (Dhall.Core.pretty result)
        V.Failure errors -> print errors
    Run (RunArgs {..}) -> do
      rules <- map convert <$> Dhall.inputFile (Dhall.auto @[DhallTypes.Rule]) rulesFile
      maybe id withCurrentDirectory runRoot $ do
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
  customExecParser (prefs showHelpOnEmpty) $
    info (argsParser <**> helper) (fullDesc <> header "Librarian: manage files, your rules!")
  where
    argsParser :: Parser Args
    argsParser =
      hsubparser
        ( command "dump-dhall-format" (info (pure Format) (progDesc "Dump Dhall format"))
            <> command "run" (info (Run <$> runP) (progDesc "Run the rules"))
        )
    runP :: Parser RunArgs
    runP =
      RunArgs
        <$> strOption (long "rules" <> short 'r' <> metavar "FILE_PATH" <> help "Rules file (.dhall)")
        <*> optional (strOption $ long "root" <> short 'd' <> metavar "ROOT_DIRECTORY" <> help "Root directory where the rules are ran (not read)")

data Args = Format | Run RunArgs
  deriving stock (Eq, Ord, Show)

data RunArgs = RunArgs
  { rulesFile :: FilePath,
    runRoot :: Maybe FilePath
  }
  deriving stock (Eq, Ord, Show)
