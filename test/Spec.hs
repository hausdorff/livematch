import Prelude hiding (Left, Right)
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Match

main :: IO ()
main = hspec $ do
  describe "index" $ do
    let empty = M.empty
    it "indexes trivial corpus" $ do
      let idx = index ["abc"]
      let target1 = M.singleton 0 (S.singleton 0)
      let target2 = M.singleton 0 (S.singleton 1)
      let target3 = M.singleton 0 (S.singleton 2)

      query idx "a" `shouldBe` target1
      query idx "ab" `shouldBe` target1
      query idx "b" `shouldBe` target2
      query idx "bc" `shouldBe` target2
      query idx "c" `shouldBe` target3

      query idx "z" `shouldBe` empty
      query idx "az" `shouldBe` empty
      query idx "za" `shouldBe` empty
      query idx "abcd" `shouldBe` empty

    it "indexes multi-document corpus" $ do
      let idx = index ["abc", "bcd"]
      let target1 = M.fromList [(0, S.singleton 1), (1, S.singleton 0)]
      let target2 = M.fromList [(0, S.singleton 2), (1, S.singleton 1)]
      let target3 = M.singleton 0 (S.singleton 0)
      let target4 = M.singleton 1 (S.singleton 0)

      query idx "b" `shouldBe` target1
      query idx "bc" `shouldBe` target1
      query idx "c" `shouldBe` target2
      query idx "ab" `shouldBe` target3
      query idx "bcd" `shouldBe` target4

      query idx "z" `shouldBe` empty
      query idx "az" `shouldBe` empty
      query idx "za" `shouldBe` empty
      query idx "abcd" `shouldBe` empty