{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LibrarianSpec
  ( main,
    spec,
  )
where

import Control.Monad
import Data.Function (on)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.String (IsString (..))
import Data.Time (addUTCTime, getCurrentTime, nominalDay, secondsToNominalDiffTime)
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
    describe "time-based" $ do
      let timeBased = fetchRulesOn "." [removeAllMidOldTtxt] >>= runPlan . planActions
      it "Should keep older and younger file" $
        withFiles
          [ "in/1.txt" {modificationTime = Just $ DaysAgo 1},
            "in/7.txt" {modificationTime = Just $ DaysAgo 7},
            "in/32.txt" {modificationTime = Just $ DaysAgo 32},
            "in/32a.txt" {modificationTime = Just $ DaysAgo 32},
            "in/32b.txt" {modificationTime = Just $ DaysAgo 32},
            "in/101.txt" {modificationTime = Just $ DaysAgo 101}
          ]
          (timeBased >> listFiles)
          `shouldReturn` ["./in/1.txt", "./in/101.txt", "./in/32.txt", "./in/7.txt"]

-- * Utils

data FileSpec = FileSpec
  { path :: FilePath,
    accessTime :: Maybe TimeSpec,
    modificationTime :: Maybe TimeSpec
  }
  deriving stock (Eq, Show)

instance IsString FileSpec where
  fromString p =
    FileSpec {path = p, accessTime = Nothing, modificationTime = Nothing}

withFiles :: [FileSpec] -> IO a -> IO a
withFiles files act =
  withSystemTempDirectory "librarian-tests" $ \d ->
    withCurrentDirectory d $ do
      mapM_ touch files
      act

touch :: FileSpec -> IO ()
touch target = do
  createDirectoryIfMissing True $ fst $ splitFileName target.path
  writeFile target.path "-"
  let computeTime =
        \case
          HoursAgo d -> addUTCTime (secondsToNominalDiffTime $ (-1) * fromInteger d * 60 * 60) <$> getCurrentTime
          DaysAgo d -> addUTCTime ((-1) * fromInteger d * nominalDay) <$> getCurrentTime
          AbsoluteTime x -> return x
  forM_ target.accessTime $ setAccessTime target.path <=< computeTime
  forM_ target.modificationTime $ setModificationTime target.path <=< computeTime

listFiles :: IO [FilePath]
listFiles = glob "./**/*" >>= fmap sort . filterM doesFileExist

instance Eq Rule where
  (==) = (==) `on` show

instance Eq ResolvedAction where
  (==) = (==) `on` show

-- * Fixtures

-- ** move

moveRules :: [Rule]
moveRules = [moveRule0Jpg, moveRule1Any]

moveRule0Jpg :: Rule
moveRule0Jpg =
  Rule
    { name = "Image files (move)",
      match = "**/*.jpg",
      grouping = FileGroup,
      filtering = AllF,
      actions = [Move "^.*/([^\\/]+)$" "out/pics/\\1"]
    }

moveRule1Any :: Rule
moveRule1Any =
  Rule
    { name = "All files (move)",
      match = "**/*",
      grouping = FileGroup,
      filtering = AllF,
      actions = [Move "pdf$" "PDF", Move "txt$" "TXT", Move "txt$" "TxT"]
    }

moveAllTxtRule :: Rule
moveAllTxtRule =
  Rule
    { name = "Text files (move)",
      match = "**/*.txt",
      grouping = FileGroup,
      filtering = AllF,
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
      grouping = FileGroup,
      filtering = AllF,
      actions = [Copy "^.*/([^\\/]+)$" "out/pics/\\1"]
    }

copyRule1Any :: Rule
copyRule1Any =
  Rule
    { name = "All files (copy)",
      match = "**/*",
      grouping = FileGroup,
      filtering = AllF,
      actions = [Copy "pdf$" "PDF", Copy "txt$" "TXT", Copy "txt$" "TxT"]
    }

copyAllTxtRule :: Rule
copyAllTxtRule =
  Rule
    { name = "Text files (copy)",
      match = "**/*.txt",
      grouping = FileGroup,
      filtering = AllF,
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
      grouping = FileGroup,
      filtering = AllF,
      actions = [Remove "^.*/([^\\/]+)$"]
    }

removeRule1Any :: Rule
removeRule1Any =
  Rule
    { name = "All files (remove)",
      match = "**/*",
      grouping = FileGroup,
      filtering = AllF,
      actions = [Remove "pdf$", Remove "txt$", Remove "txt$"]
    }

removeAllTxtRule :: Rule
removeAllTxtRule =
  Rule
    { name = "Text files (remove)",
      match = "**/*.txt",
      grouping = FileGroup,
      filtering = AllF,
      actions = [Remove "^.*/([^\\/]+)$"]
    }

removeAllMidOldTtxt :: Rule
removeAllMidOldTtxt =
  Rule
    { name = "Purge not-too-old text files",
      match = "**/*.txt",
      grouping =
        Group
          { groupSource = SourceDate ModificationTime,
            groupBucket = Monthly,
            groupSelection = After 0 SortingAsc (SourceDate ModificationTime)
          },
      filtering =
        LtF (SourceDate ModificationTime) (SourceTime $ DaysAgo 31)
          `AndF` GtF (SourceDate ModificationTime) (SourceTime $ DaysAgo 92),
      actions = [Remove "^.*/([^\\/]+)$"]
    }
