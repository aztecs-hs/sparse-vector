module Main (main) where

import qualified Data.SparseVector as SV
import Test.Hspec

main :: IO ()
main =
  hspec . describe "Data.SparseVector.lookup" $
    it "performs lookup" test_lookup

test_lookup :: Expectation
test_lookup = do
  SV.lookup 0 xs `shouldBe` Just 100
  SV.lookup 1 xs `shouldBe` Just 101
  SV.lookup 2 xs `shouldBe` Nothing
  SV.lookup 3 xs `shouldBe` Just 102
  where
    xs =
      SV.fromList
        [ (0 :: Int, 100 :: Int),
          (1 :: Int, 101 :: Int),
          (3 :: Int, 102 :: Int)
        ]
