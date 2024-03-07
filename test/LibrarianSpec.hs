module LibrarianSpec
  ( main,
    spec,
  )
where

import Control.Monad
import Data.List (sort)
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
    describe "move" $ do
      it "Empty target directory should be empty" $
        withFiles [] (fetchRulesOn "." moveRules)
          `shouldReturn` mempty
      it "Text files only should match only all rule" $
        withFiles ["in/sub/0.txt", "in/1.txt"] (fetchRulesOn "." moveRules)
          `shouldReturn` Map.fromList [("./in/sub/0.txt", [moveRule1Any]), ("./in/1.txt", [moveRule1Any])]
      it "Text/images files should match twice" $
        withFiles ["in/sub/0.jpg", "in/1.txt"] (fetchRulesOn "." moveRules)
          `shouldReturn` Map.fromList [("./in/sub/0.jpg", [moveRule0Jpg, moveRule1Any]), ("./in/1.txt", [moveRule1Any])]
    describe "copy" $ do
      it "Empty target directory should be empty" $
        withFiles [] (fetchRulesOn "." copyRules)
          `shouldReturn` mempty
      it "Text files only should match only all rule" $
        withFiles ["in/sub/0.txt", "in/1.txt"] (fetchRulesOn "." copyRules)
          `shouldReturn` Map.fromList [("./in/sub/0.txt", [copyRule1Any]), ("./in/1.txt", [copyRule1Any])]
      it "Text/images files should match twice" $
        withFiles ["in/sub/0.jpg", "in/1.txt"] (fetchRulesOn "." copyRules)
          `shouldReturn` Map.fromList [("./in/sub/0.jpg", [copyRule0Jpg, copyRule1Any]), ("./in/1.txt", [copyRule1Any])]
    describe "remove" $ do
      it "Empty target directory should be empty" $
        withFiles [] (fetchRulesOn "." removeRules)
          `shouldReturn` mempty
      it "Text files only should match only all rule" $
        withFiles ["in/sub/0.txt", "in/1.txt"] (fetchRulesOn "." removeRules)
          `shouldReturn` Map.fromList [("./in/sub/0.txt", [removeRule1Any]), ("./in/1.txt", [removeRule1Any])]
      it "Text/images files should match twice" $
        withFiles ["in/sub/0.jpg", "in/1.txt"] (fetchRulesOn "." removeRules)
          `shouldReturn` Map.fromList [("./in/sub/0.jpg", [removeRule0Jpg, removeRule1Any]), ("./in/1.txt", [removeRule1Any])]
  describe "planActions" $ do
    describe "move" $ do
      it "Images should be moved, texts should have their extension changed" $
        planActions (Map.fromList [("./in/sub/0.jpg", [moveRule0Jpg]), ("./in/1.txt", [moveRule1Any])])
          `shouldBe` [ ResolvedMove "./in/1.txt" "./in/1.TXT" moveRule1Any,
                       ResolvedMove "./in/sub/0.jpg" "out/pics/0.jpg" moveRule0Jpg
                     ]
      it "Non-matching action should be nothing" $
        planActions (Map.fromList [("./in/1.png", [moveRule1Any])])
          `shouldBe` []
    describe "copy" $ do
      it "Images should be copyd, texts should have their extension changed" $
        planActions (Map.fromList [("./in/sub/0.jpg", [copyRule0Jpg]), ("./in/1.txt", [copyRule1Any])])
          `shouldBe` [ ResolvedCopy "./in/1.txt" "./in/1.TXT" copyRule1Any,
                       ResolvedCopy "./in/sub/0.jpg" "out/pics/0.jpg" copyRule0Jpg
                     ]
      it "Non-matching action should be nothing" $
        planActions (Map.fromList [("./in/1.png", [copyRule1Any])])
          `shouldBe` []
    describe "remove" $ do
      it "Images should be removed" $
        planActions (Map.fromList [("./in/sub/0.jpg", [removeRule0Jpg]), ("./in/1.txt", [removeRule1Any])])
          `shouldBe` [ ResolvedRemove "./in/1.txt" removeRule1Any,
                       ResolvedRemove "./in/sub/0.jpg" removeRule0Jpg
                     ]
      it "Non-matching action should be nothing" $
        planActions (Map.fromList [("./in/1.png", [removeRule1Any])])
          `shouldBe` []
    describe "mixed" $ do
      it "Images should be copied and removed, texts should have their extension changed" $
        planActions (Map.fromList [("./in/sub/0.jpg", [copyRule0Jpg, removeRule0Jpg])])
          `shouldBe` [ ResolvedCopy "./in/sub/0.jpg" "out/pics/0.jpg" copyRule0Jpg,
                       ResolvedRemove "./in/sub/0.jpg" removeRule0Jpg
                     ]
  describe "runPlan" $ do
    describe "move" $ do
      let moveAll = fetchRulesOn "." [moveAllTxtRule] >>= runPlan . planActions
      it "Overriding paths should block the second move" $
        withFiles ["in/0.txt", "in/sub/0.txt"] moveAll
          `shouldReturn` [ (FsMove "./in/0.txt" "out/0.txt", Done),
                           (FsMove "./in/sub/0.txt" "out/0.txt", Existing)
                         ]
      it "Overriding paths should keep the second file" $
        withFiles ["in/0.txt", "in/sub/0.txt"] (moveAll >> listFiles)
          `shouldReturn` ["./in/sub/0.txt", "./out/0.txt"]
    describe "copy" $ do
      let copyAll = fetchRulesOn "." [copyAllTxtRule] >>= runPlan . planActions
      it "Overriding paths should block the second copy" $
        withFiles ["in/0.txt", "in/sub/0.txt"] copyAll
          `shouldReturn` [ (FsCopy "./in/0.txt" "out/0.txt", Done),
                           (FsCopy "./in/sub/0.txt" "out/0.txt", Existing)
                         ]
      it "Overriding paths should keep the second file" $
        withFiles ["in/0.txt", "in/sub/0.txt"] (copyAll >> listFiles)
          `shouldReturn` ["./in/0.txt", "./in/sub/0.txt", "./out/0.txt"]
    describe "remove" $ do
      let removeAll = fetchRulesOn "." [removeAllTxtRule] >>= runPlan . planActions
      it "Should keep the non-matching file" $
        withFiles ["in/0.txt", "in/sub/0.txt", "in/0.jpg"] removeAll
          `shouldReturn` [ (FsRemove "./in/0.txt", Done),
                           (FsRemove "./in/sub/0.txt", Done)
                         ]
      it "Should keep the non-matching file" $
        withFiles ["in/0.txt", "in/sub/0.txt", "in/0.jpg"] (removeAll >> listFiles)
          `shouldReturn` ["./in/0.jpg"]
    describe "mixed" $ do
      let mixed = fetchRulesOn "." [copyAllTxtRule, removeAllTxtRule] >>= runPlan . planActions
      it "Should copy and delete" $
        withFiles ["in/0.txt"] (mixed >> listFiles)
          `shouldReturn` ["./out/0.txt"]

-- * Utils

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

listFiles :: IO [FilePath]
listFiles = glob "./**/*" >>= fmap sort . filterM doesFileExist

-- * Fixtures

-- ** move

moveRules :: [Rule]
moveRules = [moveRule0Jpg, moveRule1Any]

moveRule0Jpg :: Rule
moveRule0Jpg =
  Rule
    { name = "Image files (move)",
      match = "**/*.jpg",
      actions = [Move "^.*/([^\\/]+)$" "out/pics/\\1"]
    }

moveRule1Any :: Rule
moveRule1Any =
  Rule
    { name = "All files (move)",
      match = "**/*",
      actions = [Move "pdf$" "PDF", Move "txt$" "TXT", Move "txt$" "TxT"]
    }

moveAllTxtRule :: Rule
moveAllTxtRule =
  Rule
    { name = "Text files (move)",
      match = "**/*.txt",
      actions = [Move "^.*/([^\\/]+)$" "out/\\1"]
    }

-- ** copy

copyRules :: [Rule]
copyRules = [copyRule0Jpg, copyRule1Any]

copyRule0Jpg :: Rule
copyRule0Jpg =
  Rule
    { name = "Image files (copy)",
      match = "**/*.jpg",
      actions = [Copy "^.*/([^\\/]+)$" "out/pics/\\1"]
    }

copyRule1Any :: Rule
copyRule1Any =
  Rule
    { name = "All files (copy)",
      match = "**/*",
      actions = [Copy "pdf$" "PDF", Copy "txt$" "TXT", Copy "txt$" "TxT"]
    }

copyAllTxtRule :: Rule
copyAllTxtRule =
  Rule
    { name = "Text files (copy)",
      match = "**/*.txt",
      actions = [Copy "^.*/([^\\/]+)$" "out/\\1"]
    }

-- ** remove

removeRules :: [Rule]
removeRules = [removeRule0Jpg, removeRule1Any]

removeRule0Jpg :: Rule
removeRule0Jpg =
  Rule
    { name = "Image files (remove)",
      match = "**/*.jpg",
      actions = [Remove "^.*/([^\\/]+)$"]
    }

removeRule1Any :: Rule
removeRule1Any =
  Rule
    { name = "All files (remove)",
      match = "**/*",
      actions = [Remove "pdf$", Remove "txt$", Remove "txt$"]
    }

removeAllTxtRule :: Rule
removeAllTxtRule =
  Rule
    { name = "Text files (remove)",
      match = "**/*.txt",
      actions = [Remove "^.*/([^\\/]+)$"]
    }
