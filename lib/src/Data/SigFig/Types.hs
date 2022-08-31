{-# LANGUAGE ImportQualifiedPost #-}

module Data.SigFig.Types
  ( Term (..),
    Sign (..),
    Op (..),
    Expr (..),
    Function (..),
    l,
    lMeasured,
    lConstant,
    add,
    sub,
    mul,
    div,
    exp,
    apply,
    measured,
    constant,
  )
where

import Data.BigDecimal (BigDecimal (..))
import Data.BigDecimal qualified as BD
import Prelude hiding (div, exp)

-- | The basic datatype to represent measurements, constant terms, and evaluation results
data Term
  = -- | A measured value with a finite number of significant figures and an associated value
    Measured {numSigFigs :: Integer, value :: BigDecimal}
  | -- | A constant value with infinite significant figures
    Constant Rational
  deriving (Show, Eq)

measured :: Integer -> Rational -> Term
measured sf = Measured sf . fromRational

constant :: Rational -> Term
constant = Constant

data Sign = Positive | Negative
  deriving (Show, Eq)

data Op
  = Add
  | Sub
  | Mul
  | Div
  deriving (Show, Eq)

l :: Term -> Expr
l = Leaf

lMeasured :: Integer -> Rational -> Expr
lMeasured = (l .) . measured

lConstant :: Rational -> Expr
lConstant = l . constant

-- | Add together a list of 'Expr's and create a new `Expr`
add :: [Expr] -> Expr
add = Prec1 . zip (repeat Add)

sub :: [Expr] -> Expr
sub [] = Prec1 []
sub (x:xs) = Prec1 $ (Add, x) : zip (repeat Sub) xs

mul :: [Expr] -> Expr
mul = Prec2 . zip (repeat Mul)

div :: [Expr] -> Expr
div [] = Prec2 []
div (x:xs) = Prec2 $ (Mul, x) : zip (repeat Div) xs

exp :: Expr -> Integer -> Expr
exp = Exp

apply :: Function -> Expr -> Expr
apply = Apply

data Function = Log10 | Antilog10
  deriving (Show, Eq)

-- A datatype to represent (not-yet-evaluated) expressions. Use 'parse' to create such an expression from text.
data Expr
  = -- | Leaf of an expression
    Leaf Term
  | -- | Operation of "Precedence 1": addition and subtraction
    Prec1 [(Op, Expr)]
  | -- | Operation of "Precedence 2": multiplication and division
    Prec2 [(Op, Expr)]
  | -- | Exponentiation with a constant integer exponent
    Exp Expr Integer
  | -- | Application of a function to an expression argument
    Apply Function Expr
  deriving (Show, Eq)
