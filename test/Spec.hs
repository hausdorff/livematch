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
      let target2 = M.singleton 0 (S.fromList [0])
      let target3 = M.singleton 0 (S.fromList [1])
      let target4 = M.singleton 0 (S.fromList [1])
      let target5 = M.singleton 0 (S.fromList [2])

      query idx "a" `shouldBe` target1
      query idx "ab" `shouldBe` target2
      query idx "b" `shouldBe` target3
      query idx "bc" `shouldBe` target4
      query idx "c" `shouldBe` target5

      query idx "z" `shouldBe` empty
      query idx "az" `shouldBe` empty
      query idx "za" `shouldBe` empty
      query idx "abcd" `shouldBe` empty