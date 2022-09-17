{-# LANGUAGE ImportQualifiedPost #-}

-- |
-- Basic types and convenience functions for constructing your own terms and expression trees.
module Data.SigFig.Types
  ( Term (..),
    Op (..),
    Expr (..),
    Function (..),

    -- * Creating Terms and Expression Trees
    measured,
    constant,
    l,
    lMeasured,
    lConstant,

    -- * Building and Combining Expression Trees
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

-- | Create a measured value
measured :: Integer -> Rational -> Term
measured sf = Measured sf . fromRational

-- | Create a constant value
constant :: Rational -> Term
constant = Constant

-- | The types of (infix) operators
data Op
  = Add
  | Sub
  | Mul
  | Div
  deriving (Show, Eq)

-- | Create a leaf node out of a term, like a "singleton".
l :: Term -> Expr
l = Literal

-- | Create a leaf node and construct the 'Measured' value argument at the same time. Convenience function.
lMeasured :: Integer -> Rational -> Expr
lMeasured = (l .) . measured

-- | Create a leaf node and construct the 'Constant' value argument at the same time. Convenience function.
lConstant :: Rational -> Expr
lConstant = l . constant

-- | Add together a list of 'Expr's and create a new 'Expr'.
--
-- @add a b c@ is similar in idea to @a + b + c@.
add :: [Expr] -> Expr
add = Prec1 . zip (repeat Add)

-- | "Subtract together" a list of 'Expr's and create a new 'Expr'.
--
-- @sub a b c@ is similar in idea to @a - b - c@.
sub :: [Expr] -> Expr
sub [] = Prec1 []
sub (x : xs) = Prec1 $ (Add, x) : zip (repeat Sub) xs

-- | multiply together a list of 'Expr's and create a new 'Expr'.
--
-- @mul a b c@ is similar in idea to @a * b * c@.
mul :: [Expr] -> Expr
mul = Prec2 . zip (repeat Mul)

-- | "Divide together" a list of 'Expr's and create a new 'Expr'.
--
-- @div a b c@ is similar in idea to @a \/ b \/ c@.
div :: [Expr] -> Expr
div [] = Prec2 []
div (x : xs) = Prec2 $ (Mul, x) : zip (repeat Div) xs

-- | Take an 'Expr' to the power of an integer. Equivalent to 'Exp'.
exp :: Expr -> Expr -> Expr
exp = Exp

-- | Apply a function to an 'Expr'. Equivalent to 'Apply'.
apply :: Function -> Expr -> Expr
apply = Apply

-- | A datatype representing the supported functions.
data Function
  = -- | The function @log()@ in expressions.
    Log10
  | -- | The function @exp()@ in expressions.
    Antilog10
  deriving (Show, Eq)

-- | A datatype to represent (not-yet-evaluated) expressions. Use 'Data.SigFig.Parse.parse' to create such an expression from text.
data Expr
  = -- | Leaf of an expression
    Literal Term
  | -- | Operation of "Precedence 1": addition and subtraction
    Prec1 [(Op, Expr)]
  | -- | Operation of "Precedence 2": multiplication and division
    Prec2 [(Op, Expr)]
  | -- | Exponentiation with a constant exponent
    Exp Expr Expr
  | -- | Application of a function to an expression argument
    Apply Function Expr
  deriving (Show, Eq)
