{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.BigDecimal
import Data.Ratio
import SigFig
import Test.Tasty
import Test.Tasty.HUnit

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
      orderOfOperations,
      complexExpressions
    ]

singleTermTests :: TestTree
singleTermTests =
  testGroup
    "basic single terms"
    [ testCase "parse positive integer" $
        maybeParseEval "2" @?= Just (mkSFMeasured 1 2 0),
      testCase "parse negative integer" $
        maybeParseEval "-3" @?= Just (mkSFMeasured 1 (-3) 0),
      testCase "parse sci-not integer" $
        maybeParseEval "-5e7" @?= Just (mkSFMeasured 1 (-5) (-7)),
      testCase "parse sci-not float" $
        maybeParseEval "5.24e-2" @?= Just (mkSFMeasured 3 524 4)
    ]

singleConstantTests :: TestTree
singleConstantTests =
  testGroup
    "basic single constants"
    [ testCase "parse positive integer constant" $
        maybeParseEval "2c" @?= Just (SFConstant 2),
      testCase "parse negative integer constant" $
        maybeParseEval "-3c" @?= Just (SFConstant (-3)),
      testCase "parse sci-not integer constant" $
        maybeParseEval "-5e7c" @?= Just (SFConstant ((-5) * 10 ^ 7)),
      testCase "parse sci-not float constant" $
        maybeParseEval "5.24e-2c" @?= Just (SFConstant ((5.24 / 10 ^ 2) :: Rational))
    ]

singleTermParenTests :: TestTree
singleTermParenTests =
  testGroup
    "single terms with parens"
    [ testCase "parse positive integer" $
        maybeParseEval "(2)" @?= Just (mkSFMeasured 1 2 0),
      testCase "parse negative integer w/ double parens" $
        maybeParseEval "((-3))" @?= Just (mkSFMeasured 1 (-3) 0),
      testCase "parse sci-not integer w/ spaces" $
        maybeParseEval "(-5e7   )" @?= Just (mkSFMeasured 1 (-5) (-7)),
      testCase "parse sci-not float w/ double parens and spaces" $
        maybeParseEval "(  (5.24e-2) )" @?= Just (mkSFMeasured 3 524 4),
      testCase "parse sci-not constant w/ spaces" $
        maybeParseEval "(-.51e7c )" @?= Just (SFConstant $ (-51) * 10 ^ 5),
      testCase "parse sci-not constant w/ double parens and spaces" $
        maybeParseEval "(  (2.e-2c) )" @?= Just (SFConstant 0.02)
    ]

constantOpTests :: TestTree
constantOpTests =
  testGroup
    "constant operations"
    [ testCase "add two constant ints" $
        maybeParseEval "2c + 25c" @?= Just (SFConstant 27),
      testCase "add constant int and constant float" $
        maybeParseEval "0.2c + 400c" @?= Just (SFConstant 400.2),
      testCase "divide constant int and constant float" $
        maybeParseEval "1c / 3.0c" @?= Just (SFConstant (1 % 3))
    ]

singleOpTests :: TestTree
singleOpTests =
  testGroup
    "single operations"
    [ testCase "parse integer addition" $
        maybeParseEval "2 + 3" @?= Just (mkSFMeasured 1 5 0),
      testCase "parse float subtraction" $
        maybeParseEval "3.2 - 4.3" @?= Just (mkSFMeasured 2 (-11) 1),
      testCase "parse multiplication" $
        maybeParseEval "3 * 2" @?= Just (mkSFMeasured 1 6 0),
      testCase "parse division with parens" $
        maybeParseEval "(4 / 2) / 2" @?= Just (mkSFMeasured 1 1 0),
      testCase "parse exponentiation" $
        maybeParseEval "2 ** 3" @?= Just (mkSFMeasured 1 8 0)
    ]

orderOfOperations :: TestTree
orderOfOperations =
  testGroup
    "order of operations"
    [ testCase "addition after multiplication" $
        maybeParseEval "1 + 3 * 2" @?= Just (mkSFMeasured 1 7 0),
      testCase "multiplication before subtraction" $
        maybeParseEval "2.1 * 2.0 - 0.3" @?= Just (mkSFMeasured 2 39 1),
      testCase "exp > mul > add" $
        maybeParseEval "2.1 + 2.0 * 1.4 ** 2" @?= Just (mkSFMeasured 2 61 1),
      testCase "division after exponentiation" $
        maybeParseEval "4 / 2 ** 2" @?= Just (mkSFMeasured 1 1 0),
      testCase "simple as can be" $
        maybeParseEval "(2) * 4 - 1" @?= Just (mkSFMeasured 1 7 0)
    ]

complexExpressions :: TestTree
complexExpressions =
  testGroup
    "complex expressions"
    [ testCase "complex 1 with rounding" $
        maybeParseEval "(2 + 3.8 * 4.1) ** 2 - 20" @?= Just (mkSFMeasured 2 300 0),
      testCase "mix constants with measured" $
        maybeParseEval "2.0001 * 4c + 18.000007c" @?= Just (mkSFMeasured 6 260004 4),
      testCase "mix constants with measured 2" $
        maybeParseEval "4.01c + 28.4c + 18.12412" @?= Just (mkSFMeasured 7 5053412 5)
    ]
