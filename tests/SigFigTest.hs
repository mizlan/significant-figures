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
        parseEval "2" @?= Right (mkSFMeasured 1 2 0),
      testCase "parse negative integer" $
        parseEval "-3" @?= Right (mkSFMeasured 1 (-3) 0),
      testCase "parse sci-not integer" $
        parseEval "-5e7" @?= Right (mkSFMeasured 1 (-5) (-7)),
      testCase "parse sci-not float" $
        parseEval "5.24e-2" @?= Right (mkSFMeasured 3 524 4)
    ]

singleConstantTests :: TestTree
singleConstantTests =
  testGroup
    "basic single constants"
    [ testCase "parse positive integer constant" $
        parseEval "2c" @?= Right (SFConstant 2),
      testCase "parse negative integer constant" $
        parseEval "-3c" @?= Right (SFConstant (-3)),
      testCase "parse sci-not integer constant" $
        parseEval "-5e7c" @?= Right (SFConstant ((-5) * 10 ^ 7)),
      testCase "parse sci-not float constant" $
        parseEval "5.24e-2c" @?= Right (SFConstant ((5.24 / 10 ^ 2) :: Rational))
    ]

singleTermParenTests :: TestTree
singleTermParenTests =
  testGroup
    "single terms with parens"
    [ testCase "parse positive integer" $
        parseEval "(2)" @?= Right (mkSFMeasured 1 2 0),
      testCase "parse negative integer w/ double parens" $
        parseEval "((-3))" @?= Right (mkSFMeasured 1 (-3) 0),
      testCase "parse sci-not integer w/ spaces" $
        parseEval "(-5e7   )" @?= Right (mkSFMeasured 1 (-5) (-7)),
      testCase "parse sci-not float w/ double parens and spaces" $
        parseEval "(  (5.24e-2) )" @?= Right (mkSFMeasured 3 524 4),
      testCase "parse sci-not constant w/ spaces" $
        parseEval "(-.51e7c )" @?= Right (SFConstant $ (-51) * 10 ^ 5),
      testCase "parse sci-not constant w/ double parens and spaces" $
        parseEval "(  (2.e-2c) )" @?= Right (SFConstant 0.02)
    ]

constantOpTests :: TestTree
constantOpTests =
  testGroup
    "constant operations"
    [ testCase "add two constant ints" $
        parseEval "2c + 25c" @?= Right (SFConstant 27),
      testCase "add constant int and constant float" $
        parseEval "0.2c + 400c" @?= Right (SFConstant 400.2),
      testCase "divide constant int and constant float" $
        parseEval "1c / 3.0c" @?= Right (SFConstant (1 % 3))
    ]

singleOpTests :: TestTree
singleOpTests =
  testGroup
    "single operations"
    [ testCase "parse integer addition" $
        parseEval "2 + 3" @?= Right (mkSFMeasured 1 5 0),
      testCase "parse float subtraction" $
        parseEval "3.2 - 4.3" @?= Right (mkSFMeasured 2 (-11) 1),
      testCase "parse multiplication" $
        parseEval "3 * 2" @?= Right (mkSFMeasured 1 6 0),
      testCase "parse division with parens" $
        parseEval "(4 / 2) / 2" @?= Right (mkSFMeasured 1 1 0),
      testCase "parse exponentiation" $
        parseEval "2 ** 3" @?= Right (mkSFMeasured 1 8 0)
    ]

orderOfOperations :: TestTree
orderOfOperations =
  testGroup
    "order of operations"
    [ testCase "addition after multiplication" $
        parseEval "1 + 3 * 2" @?= Right (mkSFMeasured 1 7 0),
      testCase "multiplication before subtraction" $
        parseEval "2.1 * 2.0 - 0.3" @?= Right (mkSFMeasured 2 39 1),
      testCase "exp > mul > add" $
        parseEval "2.1 + 2.0 * 1.4 ** 2" @?= Right (mkSFMeasured 2 61 1),
      testCase "division after exponentiation" $
        parseEval "4 / 2 ** 2" @?= Right (mkSFMeasured 1 1 0),
      testCase "simple as can be" $
        parseEval "(2) * 4 - 1" @?= Right (mkSFMeasured 1 7 0)
    ]

complexExpressions :: TestTree
complexExpressions =
  testGroup
    "complex expressions"
    [ testCase "complex 1 with rounding" $
        parseEval "(2 + 3.8 * 4.1) ** 2 - 20" @?= Right (mkSFMeasured 2 300 0),
      testCase "mix constants with measured" $
        parseEval "2.0001 * 4c + 18.000007c" @?= Right (mkSFMeasured 6 260004 4),
      testCase "mix constants with measured 2" $
        parseEval "4.01c + 28.4c + 18.12412" @?= Right (mkSFMeasured 7 5053412 5),
      testCase "constant division" $
        parseEval "2c/1c/2c/1c/2c * 8" @?= Right (mkSFMeasured 1 4 0),
      testCase "constant division 2" $
        parseEval "2c/1c/2c/1c/2c * 8c" @?= Right (SFConstant 4),
      testCase "interspersed constants and measured" $
        parseEval "((2c) + 3.1 * (4.7c) ** 1)" @?= Right (mkSFMeasured 2 17 0),
      testCase "simple addition" $
        parseEval "4 + 5 + 6" @?= Right (mkSFMeasured 2 15 0),
      testCase "simple addition with mul" $
        parseEval "(4 + 5 + 6) * 1c" @?= Right (mkSFMeasured 2 15 0),
      testCase "not-so-simple addition" $
        parseEval "(4 + 5 + 6) * 1" @?= Right (mkSFMeasured 1 20 0)
    ]
