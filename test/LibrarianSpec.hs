module LibrarianSpec
  ( main,
    spec,
  )
where

import Librarian
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "failing test" $
    True `shouldBe` False
