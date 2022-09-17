{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.BigDecimal
import Data.Either (isLeft)
import Data.Ratio
import Data.SigFig
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Natural (naturalFromInteger)
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (div, exp)

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
      complexExpressions,
      createExprTests
    ]

-- | Cheese testing by copypasting from repl
verbatim :: TestName -> Text -> Text -> TestTree
verbatim name inputLine outputLine = testCase name $ case T.stripPrefix "expr> " inputLine of
  Nothing -> assertFailure "incorrect format to verbatim"
  Just e -> processExpression e @?= outputLine

singleTermTests :: TestTree
singleTermTests =
  testGroup
    "basic single terms"
    [ testCase "parse positive integer" $
        parseEval "2" @?= Right (measured 1 2),
      testCase "parse negative integer" $
        parseEval "-3" @?= Right (measured 1 (-3)),
      testCase "parse sci-not integer" $
        parseEval "-5e7" @?= Right (measured 1 (-5e7)),
      testCase "parse sci-not float" $
        parseEval "5.24e-2" @?= Right (measured 3 0.0524)
    ]

prettyPrintTests :: TestTree
prettyPrintTests =
  testGroup
    "test displayFull"
    [ testCase "prints 0 correctly" $
        displayFull (Measured 1 (BigDecimal 0 0)) @?= "0 (1 s.f.)",
      testCase "prints trailing dot correctly" $
        displayFull (Measured 3 (BigDecimal 200 0)) @?= "200. (3 s.f.)",
      testCase "prints trailing dot and zeroes correctly" $
        displayFull (Measured 3 (BigDecimal 4 0)) @?= "4.00 (3 s.f.)",
      testCase "prints scinot correctly" $
        displayFull (Measured 2 (BigDecimal 400 0)) @?= "4.0 x 10^2 (2 s.f.)",
      testCase "prints scinot correctly 2" $
        displayFull (Measured 2 (BigDecimal 430 0)) @?= "430 (2 s.f.)",
      testCase "prints 1" $
        displayFull (Measured 1 (BigDecimal 1 0)) @?= "1 (1 s.f.)",
      testCase "terminating const" $
        displayFull (Constant (3 % 8)) @?= "0.375 (const)",
      testCase "non-terminating const" $
        displayFull (Constant (4 % 9)) @?= "4/9 (non-terminating const)",
      testCase "non-terminating const" $
        displayFull (measured 1 60) @?= "60 (1 s.f.)"
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
        parseEval "(2)" @?= Right (measured 1 2),
      testCase "parse negative integer w/ double parens" $
        parseEval "((-3))" @?= Right (measured 1 (-3)),
      testCase "parse sci-not integer w/ spaces" $
        parseEval "(-5e7   )" @?= Right (measured 1 (-5e7)),
      testCase "parse sci-not float w/ double parens and spaces" $
        parseEval "(  (5.24e-2) )" @?= Right (measured 3 0.0524),
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
        parseEval "2 + 3" @?= Right (measured 1 5),
      testCase "parse float subtraction" $
        parseEval "3.2 - 4.3" @?= Right (measured 2 (-1.1)),
      testCase "parse multiplication" $
        parseEval "3 * 2" @?= Right (measured 1 6),
      testCase "parse division with parens" $
        parseEval "(4 / 2) / 2" @?= Right (measured 1 1),
      testCase "parse exponentiation" $
        parseEval "2 ** 3" @?= Right (measured 1 8),
      testCase "parse log" $
        parseEval "log(10)" @?= Right (measured 2 1),
      testCase "parse log 2" $
        parseEval "log(log(10000000000))" @?= Right (measured 4 1)
    ]

orderOfOperations :: TestTree
orderOfOperations =
  testGroup
    "order of operations"
    [ testCase "addition after multiplication" $
        parseEval "1 + 3 * 2" @?= Right (measured 1 7),
      testCase "multiplication before subtraction" $
        parseEval "2.1 * 2.0 - 0.3" @?= Right (measured 2 3.9),
      testCase "exp > mul > add" $
        parseEval "2.1 + 2.0 * 1.4 ** 2" @?= Right (measured 2 6.1),
      testCase "division after exponentiation" $
        parseEval "4 / 2 ** 2" @?= Right (measured 1 1),
      testCase "simple as can be" $
        parseEval "(2) * 4 - 1" @?= Right (measured 1 7),
      testCase "logs first" $
        parseEval "log(10) * log(10) * 2.1" @?= Right (measured 2 2.1),
      testCase "logs last" $
        parseEval "log(2c * 7.0)" @?= Right (measured 3 1.15),
      testCase "exp log" $
        parseEval "log(10 ** 3)" @?= Right (measured 2 3),
      testCase "exp log 2" $
        (isLeft . parseEval $ "log(10c ** 3)") @? "log of constant"
    ]

complexExpressions :: TestTree
complexExpressions =
  testGroup
    "complex expressions"
    [ testCase "complex 1 with rounding" $
        parseEval "(2 + 3.8 * 4.1) ** 2 - 20" @?= Right (measured 2 300),
      testCase "mix constants with measured" $
        parseEval "2.0001 * 4c + 18.000007c" @?= Right (measured 6 26.0004),
      testCase "mix constants with measured 2" $
        parseEval "4.01c + 28.4c + 18.12412" @?= Right (measured 7 50.53412),
      testCase "constant division" $
        parseEval "2c/1c/2c/1c/2c * 8" @?= Right (measured 1 4),
      testCase "constant division 2" $
        parseEval "2c/1c/2c/1c/2c * 8c" @?= Right (Constant 4),
      testCase "interspersed constants and measured" $
        parseEval "((2c) + 3.1 * (4.7c) ** 1)" @?= Right (measured 2 17),
      testCase "simple addition" $
        parseEval "4 + 5 + 6" @?= Right (measured 2 15),
      testCase "simple addition with mul" $
        parseEval "(4 + 5 + 6) * 1c" @?= Right (measured 2 15),
      testCase "not-so-simple addition" $
        parseEval "(4 + 5 + 6) * 1" @?= Right (measured 1 20)
    ]

createExprTests =
  let addLhs = add [lMeasured 2 3.0, lConstant 4.2]
      addRhs = Prec1 [(Add, Leaf $ Measured 2 (BigDecimal 3 0)), (Add, Leaf $ Constant 4.2)]
      subLhs = sub [lMeasured 2 3.0, lConstant 4.2]
      subRhs = Prec1 [(Add, Leaf $ Measured 2 (BigDecimal 3 0)), (Sub, Leaf $ Constant 4.2)]
      mulLhs = mul [lMeasured 2 3.0, lConstant 4.2, lMeasured 4 2.2]
      mulRhs = Prec2 [(Mul, Leaf $ Measured 2 (BigDecimal 3 0)), (Mul, Leaf $ Constant 4.2), (Mul, Leaf $ Measured 4 2.2)]
      divLhs = div [lMeasured 2 3.0, lConstant 4.2]
      divRhs = Prec2 [(Mul, Leaf $ Measured 2 (BigDecimal 3 0)), (Div, Leaf $ Constant 4.2)]
      expLhs = exp mulLhs (lConstant 2)
      expRhs = Exp mulRhs (lConstant 2)
   in testGroup
        "creating expressions"
        [ testCase "basic" $
            addLhs @?= addRhs,
          testCase "subtraction" $
            subLhs @?= subRhs,
          testCase "multiplication" $
            mulLhs @?= mulRhs,
          testCase "division" $
            divLhs @?= divRhs,
          testCase "exp" $
            expLhs @?= expRhs
        ]
