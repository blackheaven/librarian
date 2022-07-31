module LibrarianSpec
  ( main,
    spec,
  )
where

import Control.Monad
import qualified Data.Map.Strict as Map
import Librarian
import System.Directory
import System.EasyFile
import System.FilePath.Glob
import System.IO.Temp
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "fetchRulesOn" $ do
    it "Empty target directory should be empty" $
      withFiles [] (fetchRulesOn "." rules)
        `shouldReturn` mempty
    it "Text files only should match only all rule" $
      withFiles ["in/sub/0.txt", "in/1.txt"] (fetchRulesOn "." rules)
        `shouldReturn` Map.fromList [("./in/sub/0.txt", rule1All), ("./in/1.txt", rule1All)]
    it "Text/images files should match by priority" $
      withFiles ["in/sub/0.jpg", "in/1.txt"] (fetchRulesOn "." rules)
        `shouldReturn` Map.fromList [("./in/sub/0.jpg", rule0Jpg), ("./in/1.txt", rule1All)]
  describe "planMoves" $ do
    it "Images should be moved, texts should have their extension changed" $
      planMoves (Map.fromList [("./in/sub/0.jpg", rule0Jpg), ("./in/1.txt", rule1All)])
        `shouldBe` [ Move "./in/1.txt" (Just "./in/1.TXT") rule1All,
                     Move "./in/sub/0.jpg" (Just "out/pics/0.jpg") rule0Jpg
                   ]
    it "Non-matching mover should be nothing" $
      planMoves (Map.fromList [("./in/1.png", rule1All)])
        `shouldBe` [Move "./in/1.png" Nothing rule1All]
  describe "runPlan" $ do
    let moveAll = fetchRulesOn "." [overridingRule] >>= runPlan . planMoves
    it "Overriding paths should block the second move" $
      withFiles ["in/0.txt", "in/sub/0.txt"] moveAll
        `shouldReturn` [ (Action "./in/0.txt" "out/0.txt", Done),
                         (Action "./in/sub/0.txt" "out/0.txt", Existing)
                       ]
    it "Overriding paths should keep the second file" $
      withFiles ["in/0.txt", "in/sub/0.txt"] (moveAll >> listFiles)
        `shouldReturn` ["./out/0.txt", "./in/sub/0.txt"]

withFiles :: [FilePath] -> IO a -> IO a
withFiles files act =
  withSystemTempDirectory "librarian-tests" $ \d ->
    withCurrentDirectory d $ do
      mapM_ touch files
      act

touch :: FilePath -> IO ()
touch target = do
  createDirectoryIfMissing True $ fst $ splitFileName target
  writeFile target "-"

rules :: [Rule]
rules = [rule0Jpg, rule1All]

rule0Jpg :: Rule
rule0Jpg =
  Rule
    { name = "Image files",
      match = "**/*.jpg",
      movers = [Mover "^.*/([^\\/]+)$" "out/pics/\\1"]
    }

rule1All :: Rule
rule1All =
  Rule
    { name = "All files",
      match = "**/*",
      movers = [Mover "pdf$" "PDF", Mover "txt$" "TXT", Mover "txt$" "TxT"]
    }

overridingRule :: Rule
overridingRule =
  Rule
    { name = "Text files",
      match = "**/*.txt",
      movers = [Mover "^.*/([^\\/]+)$" "out/\\1"]
    }

listFiles :: IO [FilePath]
listFiles = glob "./**/*" >>= filterM doesFileExist
