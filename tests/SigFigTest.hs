{-# LANGUAGE OverloadedStrings #-}

module Main where

import SigFig
import Data.BigDecimal
import Test.Tasty
import Test.Tasty.HUnit

mkSFMeasured :: Integer -> Integer -> Integer -> SFTerm
mkSFMeasured sf v s = SFMeasured sf (BigDecimal v s)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "basic tests"
 [singleTermTests,
  singleTermParenTests,
  singleOpTests,
  orderOfOperations]

singleTermTests :: TestTree
singleTermTests = testGroup "basic single terms"
  [ testCase "parse positive integer" $
      maybeParse "2" @?= Just (mkSFMeasured 1 2 0)
  , testCase "parse negative integer" $
      maybeParse "-3" @?= Just (mkSFMeasured 1 (-3) 0)
  , testCase "parse sci-not integer" $
      maybeParse "-5e7" @?= Just (mkSFMeasured 1 (-5) (-7))
  , testCase "parse sci-not float" $
      maybeParse "5.24e-2" @?= Just (mkSFMeasured 3 524 4)
  ]

singleConstantTests :: TestTree
singleConstantTests = testGroup "basic single constants"
  [ testCase "parse positive integer" $
      maybeParse "2c" @?= Just (SFConstant 2)
  , testCase "parse negative integer" $
      maybeParse "-3" @?= Just (SFConstant (-3))
  , testCase "parse sci-not integer" $
      maybeParse "-5e7c" @?= Just (SFConstant ((-5) * 10 ^ 7))
  , testCase "parse sci-not float" $
      maybeParse "5.24e-2" @?= Just (SFConstant ((5.24 * 10 ^ (-2)) :: Rational))
  ]

singleTermParenTests :: TestTree
singleTermParenTests = testGroup "single terms with parens"
  [ testCase "parse positive integer" $
      maybeParse "(2)" @?= Just (mkSFMeasured 1 2 0)
  , testCase "parse negative integer w/ double parens" $
      maybeParse "((-3))" @?= Just (mkSFMeasured 1 (-3) 0)
  , testCase "parse sci-not integer w/ spaces" $
      maybeParse "(-5e7   )" @?= Just (mkSFMeasured 1 (-5) (-7))
  , testCase "parse sci-not float w/ double parens and spaces" $
      maybeParse "(  (5.24e-2) )" @?= Just (mkSFMeasured 3 524 4)
  ]

singleOpTests :: TestTree
singleOpTests = testGroup "single operations"
  [ testCase "parse integer addition" $
      maybeParse "2 + 3" @?= Just (mkSFMeasured 1 5 0)
  , testCase "parse float subtraction" $
      maybeParse "3.2 - 4.3" @?= Just (mkSFMeasured 2 (-11) 1)
  , testCase "parse multiplication" $
      maybeParse "3 * 2" @?= Just (mkSFMeasured 1 6 0)
  , testCase "parse division with parens" $
      maybeParse "(4 / 2) / 2" @?= Just (mkSFMeasured 1 1 0)
  , testCase "parse exponentiation" $
      maybeParse "2 ** 3" @?= Just (mkSFMeasured 1 8 0)
  ]

orderOfOperations :: TestTree
orderOfOperations = testGroup "order of operations"
  [ testCase "addition after multiplication" $
      maybeParse "1 + 3 * 2" @?= Just (mkSFMeasured 1 7 0)
  , testCase "multiplication before subtraction" $
      maybeParse "2.1 * 2.0 - 0.3" @?= Just (mkSFMeasured 2 39 1)
  , testCase "exp > mul > add" $
      maybeParse "2.1 + 2.0 * 1.4 ** 2" @?= Just (mkSFMeasured 2 61 1)
  , testCase "division after exponentiation" $
      maybeParse "4 / 2 ** 2" @?= Just (mkSFMeasured 1 1 0)
  ]
