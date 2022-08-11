{-# LANGUAGE OverloadedStrings #-}

module Main where

import SigFig
import Data.BigDecimal
import Test.Tasty
import Test.Tasty.HUnit

mkSFTerm :: Integer -> Integer -> Integer -> SFTerm
mkSFTerm sf v s = SFTerm sf (BigDecimal v s)

main :: IO ()
main = defaultMain unitTests

unitTests :: TestTree
unitTests = testGroup "unit tests"
  [ testCase "parse positive integer" $
      maybeParse "2" @?= Just (mkSFTerm 1 2 0)
  , testCase "parse negative integer" $
      maybeParse "-3" @?= Just (mkSFTerm 1 (-3) 0)
  , testCase "parse sci-not integer" $
      maybeParse "-5e7" @?= Just (mkSFTerm 1 (-5) (-7))
  , testCase "parse sci-not float" $
      maybeParse "5.24e-2" @?= Just (mkSFTerm 3 524 4)
  ]
