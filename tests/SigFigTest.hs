{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.BigDecimal
import Data.Either (isLeft)
import Data.Ratio
import Data.SigFig.Interface
import Data.SigFig.Types
import Data.SigFig.Util
import Test.Tasty
import Test.Tasty.HUnit

mkMeasured :: Integer -> Integer -> Integer -> Term
mkMeasured sf v s = Measured sf (BigDecimal v s)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "basic tests"
    [ singleTermTests,
      singleTermParenTests,
      singleConstantTests,
      prettyPrintTests,
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
        parseEval "2" @?= Right (mkMeasured 1 2 0),
      testCase "parse negative integer" $
        parseEval "-3" @?= Right (mkMeasured 1 (-3) 0),
      testCase "parse sci-not integer" $
        parseEval "-5e7" @?= Right (mkMeasured 1 (-5) (-7)),
      testCase "parse sci-not float" $
        parseEval "5.24e-2" @?= Right (mkMeasured 3 524 4)
    ]

prettyPrintTests :: TestTree
prettyPrintTests =
  testGroup
    "test display"
    [ testCase "prints 0 correctly" $
        display (Measured 1 (BigDecimal 0 0)) @?= "0 (1 s.f.)",
      testCase "prints trailing dot correctly" $
        display (Measured 3 (BigDecimal 200 0)) @?= "200. (3 s.f.)",
      testCase "prints trailing dot and zeroes correctly" $
        display (Measured 3 (BigDecimal 4 0)) @?= "4.00 (3 s.f.)",
      testCase "prints scinot correctly" $
        display (Measured 2 (BigDecimal 400 0)) @?= "4.0 x 10^2 (2 s.f.)",
      testCase "prints scinot correctly 2" $
        display (Measured 2 (BigDecimal 430 0)) @?= "430 (2 s.f.)",
      testCase "prints 1" $
        display (Measured 1 (BigDecimal 1 0)) @?= "1 (1 s.f.)",
      testCase "terminating const" $
        display (Constant (3 % 8)) @?= "0.375 (const)",
      testCase "non-terminating const" $
        display (Constant (4 % 9)) @?= "4/9 (non-terminating const)",
      testCase "non-terminating const" $
        display (mkMeasured 1 60 0) @?= "60 (1 s.f.)"
    ]

singleConstantTests :: TestTree
singleConstantTests =
  testGroup
    "basic single constants"
    [ testCase "parse positive integer constant" $
        parseEval "2c" @?= Right (Constant 2),
      testCase "parse negative integer constant" $
        parseEval "-3c" @?= Right (Constant (-3)),
      testCase "parse sci-not integer constant" $
        parseEval "-5e7c" @?= Right (Constant ((-5) * 10 ^ 7)),
      testCase "parse sci-not float constant" $
        parseEval "5.24e-2c" @?= Right (Constant ((5.24 / 10 ^ 2) :: Rational))
    ]

singleTermParenTests :: TestTree
singleTermParenTests =
  testGroup
    "single terms with parens"
    [ testCase "parse positive integer" $
        parseEval "(2)" @?= Right (mkMeasured 1 2 0),
      testCase "parse negative integer w/ double parens" $
        parseEval "((-3))" @?= Right (mkMeasured 1 (-3) 0),
      testCase "parse sci-not integer w/ spaces" $
        parseEval "(-5e7   )" @?= Right (mkMeasured 1 (-5) (-7)),
      testCase "parse sci-not float w/ double parens and spaces" $
        parseEval "(  (5.24e-2) )" @?= Right (mkMeasured 3 524 4),
      testCase "parse sci-not constant w/ spaces" $
        parseEval "(-.51e7c )" @?= Right (Constant $ (-51) * 10 ^ 5),
      testCase "parse sci-not constant w/ double parens and spaces" $
        parseEval "(  (2.e-2c) )" @?= Right (Constant 0.02)
    ]

constantOpTests :: TestTree
constantOpTests =
  testGroup
    "constant operations"
    [ testCase "add two constant ints" $
        parseEval "2c + 25c" @?= Right (Constant 27),
      testCase "add constant int and constant float" $
        parseEval "0.2c + 400c" @?= Right (Constant 400.2),
      testCase "divide constant int and constant float" $
        parseEval "1c / 3.0c" @?= Right (Constant (1 % 3))
    ]

singleOpTests :: TestTree
singleOpTests =
  testGroup
    "single operations"
    [ testCase "parse integer addition" $
        parseEval "2 + 3" @?= Right (mkMeasured 1 5 0),
      testCase "parse float subtraction" $
        parseEval "3.2 - 4.3" @?= Right (mkMeasured 2 (-11) 1),
      testCase "parse multiplication" $
        parseEval "3 * 2" @?= Right (mkMeasured 1 6 0),
      testCase "parse division with parens" $
        parseEval "(4 / 2) / 2" @?= Right (mkMeasured 1 1 0),
      testCase "parse exponentiation" $
        parseEval "2 ** 3" @?= Right (mkMeasured 1 8 0),
      testCase "parse log" $
        parseEval "log(10)" @?= Right (mkMeasured 2 1 0),
      testCase "parse log 2" $
        parseEval "log(log(10000000000))" @?= Right (mkMeasured 4 1 0)
    ]

orderOfOperations :: TestTree
orderOfOperations =
  testGroup
    "order of operations"
    [ testCase "addition after multiplication" $
        parseEval "1 + 3 * 2" @?= Right (mkMeasured 1 7 0),
      testCase "multiplication before subtraction" $
        parseEval "2.1 * 2.0 - 0.3" @?= Right (mkMeasured 2 39 1),
      testCase "exp > mul > add" $
        parseEval "2.1 + 2.0 * 1.4 ** 2" @?= Right (mkMeasured 2 61 1),
      testCase "division after exponentiation" $
        parseEval "4 / 2 ** 2" @?= Right (mkMeasured 1 1 0),
      testCase "simple as can be" $
        parseEval "(2) * 4 - 1" @?= Right (mkMeasured 1 7 0),
      testCase "logs first" $
        parseEval "log(10) * log(10) * 2.1" @?= Right (mkMeasured 2 21 1),
      testCase "logs last" $
        parseEval "log(2c * 7.0)" @?= Right (mkMeasured 3 115 2),
      testCase "exp log" $
        parseEval "log(10 ** 3)" @?= Right (mkMeasured 2 3 0),
      testCase "exp log 2" $
        (isLeft . parseEval $ "log(10c ** 3)") @? "log of constant"
    ]

complexExpressions :: TestTree
complexExpressions =
  testGroup
    "complex expressions"
    [ testCase "complex 1 with rounding" $
        parseEval "(2 + 3.8 * 4.1) ** 2 - 20" @?= Right (mkMeasured 2 300 0),
      testCase "mix constants with measured" $
        parseEval "2.0001 * 4c + 18.000007c" @?= Right (mkMeasured 6 260004 4),
      testCase "mix constants with measured 2" $
        parseEval "4.01c + 28.4c + 18.12412" @?= Right (mkMeasured 7 5053412 5),
      testCase "constant division" $
        parseEval "2c/1c/2c/1c/2c * 8" @?= Right (mkMeasured 1 4 0),
      testCase "constant division 2" $
        parseEval "2c/1c/2c/1c/2c * 8c" @?= Right (Constant 4),
      testCase "interspersed constants and measured" $
        parseEval "((2c) + 3.1 * (4.7c) ** 1)" @?= Right (mkMeasured 2 17 0),
      testCase "simple addition" $
        parseEval "4 + 5 + 6" @?= Right (mkMeasured 2 15 0),
      testCase "simple addition with mul" $
        parseEval "(4 + 5 + 6) * 1c" @?= Right (mkMeasured 2 15 0),
      testCase "not-so-simple addition" $
        parseEval "(4 + 5 + 6) * 1" @?= Right (mkMeasured 1 20 0)
    ]
