{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.BigDecimal
import SigFig
import Test.Tasty
import Test.Tasty.HUnit
import Data.Ratio

mkSFMeasured :: Integer -> Integer -> Integer -> SFTerm
mkSFMeasured sf v s = SFMeasured sf (BigDecimal v s)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "basic tests"
    [ singleTermTests,
      singleTermParenTests,
      singleConstantTests,
      constantOpTests,
      singleOpTests,
      orderOfOperations
    ]

singleTermTests :: TestTree
singleTermTests =
  testGroup
    "basic single terms"
    [ testCase "parse positive integer" $
        maybeParse "2" @?= Just (mkSFMeasured 1 2 0),
      testCase "parse negative integer" $
        maybeParse "-3" @?= Just (mkSFMeasured 1 (-3) 0),
      testCase "parse sci-not integer" $
        maybeParse "-5e7" @?= Just (mkSFMeasured 1 (-5) (-7)),
      testCase "parse sci-not float" $
        maybeParse "5.24e-2" @?= Just (mkSFMeasured 3 524 4)
    ]

singleConstantTests :: TestTree
singleConstantTests =
  testGroup
    "basic single constants"
    [ testCase "parse positive integer constant" $
        maybeParse "2c" @?= Just (SFConstant 2),
      testCase "parse negative integer constant" $
        maybeParse "-3c" @?= Just (SFConstant (-3)),
      testCase "parse sci-not integer constant" $
        maybeParse "-5e7c" @?= Just (SFConstant ((-5) * 10 ^ 7)),
      testCase "parse sci-not float constant" $
        maybeParse "5.24e-2c" @?= Just (SFConstant ((5.24 / 10 ^ 2) :: Rational))
    ]

singleTermParenTests :: TestTree
singleTermParenTests =
  testGroup
    "single terms with parens"
    [ testCase "parse positive integer" $
        maybeParse "(2)" @?= Just (mkSFMeasured 1 2 0),
      testCase "parse negative integer w/ double parens" $
        maybeParse "((-3))" @?= Just (mkSFMeasured 1 (-3) 0),
      testCase "parse sci-not integer w/ spaces" $
        maybeParse "(-5e7   )" @?= Just (mkSFMeasured 1 (-5) (-7)),
      testCase "parse sci-not float w/ double parens and spaces" $
        maybeParse "(  (5.24e-2) )" @?= Just (mkSFMeasured 3 524 4),
      testCase "parse sci-not constant w/ spaces" $
        maybeParse "(-.51e7c )" @?= Just (SFConstant $ (-51) * 10 ^ 5),
      testCase "parse sci-not constant w/ double parens and spaces" $
        maybeParse "(  (2.e-2c) )" @?= Just (SFConstant 0.02)
    ]

constantOpTests :: TestTree
constantOpTests =
  testGroup
    "constant operations"
    [ testCase "add two constant ints" $
        maybeParse "2c + 25c" @?= Just (SFConstant 27),
      testCase "add constant int and constant float" $
        maybeParse "0.2c + 400c" @?= Just (SFConstant 400.2),
      testCase "divide constant int and constant float" $
        maybeParse "1c / 3.0c" @?= Just (SFConstant (1 % 3))
    ]

singleOpTests :: TestTree
singleOpTests =
  testGroup
    "single operations"
    [ testCase "parse integer addition" $
        maybeParse "2 + 3" @?= Just (mkSFMeasured 1 5 0),
      testCase "parse float subtraction" $
        maybeParse "3.2 - 4.3" @?= Just (mkSFMeasured 2 (-11) 1),
      testCase "parse multiplication" $
        maybeParse "3 * 2" @?= Just (mkSFMeasured 1 6 0),
      testCase "parse division with parens" $
        maybeParse "(4 / 2) / 2" @?= Just (mkSFMeasured 1 1 0),
      testCase "parse exponentiation" $
        maybeParse "2 ** 3" @?= Just (mkSFMeasured 1 8 0)
    ]

orderOfOperations :: TestTree
orderOfOperations =
  testGroup
    "order of operations"
    [ testCase "addition after multiplication" $
        maybeParse "1 + 3 * 2" @?= Just (mkSFMeasured 1 7 0),
      testCase "multiplication before subtraction" $
        maybeParse "2.1 * 2.0 - 0.3" @?= Just (mkSFMeasured 2 39 1),
      testCase "exp > mul > add" $
        maybeParse "2.1 + 2.0 * 1.4 ** 2" @?= Just (mkSFMeasured 2 61 1),
      testCase "division after exponentiation" $
        maybeParse "4 / 2 ** 2" @?= Just (mkSFMeasured 1 1 0)
    ]
