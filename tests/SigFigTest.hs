{-# LANGUAGE OverloadedStrings #-}

module Main where

import SigFig
import Data.BigDecimal
import Test.Tasty
import Test.Tasty.HUnit

mkSFTerm :: Integer -> Integer -> Integer -> SFTerm
mkSFTerm sf v s = SFTerm sf (BigDecimal v s)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "basic tests" [singleTermTests, singleTermParenTests, singleOpTests]

singleTermTests :: TestTree
singleTermTests = testGroup "basic single terms"
  [ testCase "parse positive integer" $
      maybeParse "2" @?= Just (mkSFTerm 1 2 0)
  , testCase "parse negative integer" $
      maybeParse "-3" @?= Just (mkSFTerm 1 (-3) 0)
  , testCase "parse sci-not integer" $
      maybeParse "-5e7" @?= Just (mkSFTerm 1 (-5) (-7))
  , testCase "parse sci-not float" $
      maybeParse "5.24e-2" @?= Just (mkSFTerm 3 524 4)
  ]

singleTermParenTests :: TestTree
singleTermParenTests = testGroup "single terms with parens"
  [ testCase "parse positive integer" $
      maybeParse "(2)" @?= Just (mkSFTerm 1 2 0)
  , testCase "parse negative integer w/ double parens" $
      maybeParse "((-3))" @?= Just (mkSFTerm 1 (-3) 0)
  , testCase "parse sci-not integer" $
      maybeParse "(-5e7)" @?= Just (mkSFTerm 1 (-5) (-7))
  , testCase "parse sci-not float w/ double parens" $
      maybeParse "((5.24e-2))" @?= Just (mkSFTerm 3 524 4)
  ]

singleOpTests :: TestTree
singleOpTests = testGroup "single operations"
  [ testCase "parse integer addition" $
      maybeParse "2 + 3" @?= Just (mkSFTerm 1 5 0)
  , testCase "parse float subtraction" $
      maybeParse "3.2 - 4.3" @?= Just (mkSFTerm 2 (-11) 1)
  , testCase "parse multiplication" $
      maybeParse "3 * 2" @?= Just (mkSFTerm 1 6 0)
  , testCase "parse division with parens" $
      maybeParse "(4 / 2) / 2" @?= Just (mkSFTerm 1 1 0)
  ]
