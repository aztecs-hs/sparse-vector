module Main where

import qualified Data.SparseVector as SV

main :: IO ()
main = print $ SV.insert 10 "B" $ SV.insert 0 "A" SV.empty
