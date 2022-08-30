{-# LANGUAGE ImportQualifiedPost #-}

module Data.SigFig.Types
  ( Term (..),
    Sign (..),
    Op (..),
    Expr (..),
    Function (..),
    l,
    add,
    sub,
    mul,
    div,
    exp,
    apply,
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

measured :: (RealFrac a) => Integer -> a -> Term
measured sf = Measured sf . realToFrac

constant :: (RealFrac a) => a -> Term
constant = Constant . toRational

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

-- | Add together a list of 'Expr's and create a new `Expr`
add :: [Expr] -> Expr
add = Prec1 . zip (repeat Add)

sub :: [Expr] -> Expr
sub = Prec1 . zip (repeat Sub)

mul :: [Expr] -> Expr
mul = Prec2 . zip (repeat Mul)

div :: [Expr] -> Expr
div = Prec2 . zip (repeat Div)

exp :: Expr -> Integer -> Expr
exp = Exp

apply :: Function -> Expr -> Expr
apply = Apply

data Function = Log10 | Antilog10
  deriving (Show)

data Expr
  = Leaf Term
  | Prec1 [(Op, Expr)]
  | Prec2 [(Op, Expr)]
  | Exp Expr Integer
  | Apply Function Expr
  deriving (Show)
