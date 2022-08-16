{-# LANGUAGE ImportQualifiedPost #-}

module Data.SigFig.Types
  ( Term (..),
    Sign (..),
    Op (..),
    Expr (..),
    Function (..),
  )
where

import Data.BigDecimal (BigDecimal (..))
import Data.BigDecimal qualified as BD

data Term
  = Measured {numSigFigs :: Integer, value :: BigDecimal}
  | Constant Rational
  deriving (Show, Eq)

data Sign = Positive | Negative
  deriving (Show, Eq)

data Op
  = Add
  | Sub
  | Mul
  | Div
  deriving (Show, Eq)

data Function = Log10 | Antilog10
  deriving (Show)

data Expr
  = Leaf Term
  | Prec1 [(Op, Expr)]
  | Prec2 [(Op, Expr)]
  | Exp Expr Integer
  | Apply Function Expr
  deriving (Show)
