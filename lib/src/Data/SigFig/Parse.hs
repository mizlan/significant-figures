{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune #-}

-- |
-- A module that exports two functions, 'parse' and 'parse'',
-- to help with turning text into expression trees.
module Data.SigFig.Parse
  ( parse,
    parse',
  )
where

import Control.Monad (when)
import Data.Bifunctor (first)
import Data.BigDecimal (BigDecimal (BigDecimal))
import Data.BigDecimal qualified as BD
import Data.Foldable (foldr')
import Data.SigFig.Types
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Real (Ratio ((:%)), (%))
import Text.Parsec hiding (parse)
import Text.Parsec qualified as P
import Prelude hiding (exponent)

type Parses = Parsec Text ()

-- | Represents signs.
data Sign = Positive | Negative
  deriving (Show, Eq)

-- | Parse text into either an error message or an expression.
parse :: Text -> Either Text Expr
parse = textify . P.parse expr ""
  where
    textify = first (T.pack . show)

-- | Like 'parse', but assume the result is a valid expression and crash otherwise.
parse' :: Text -> Expr
parse' s = case parse s of
  Left e -> error . T.unpack $ "parse' crashed because: " <> e
  Right e -> e

toOp :: Char -> Op
toOp '+' = Add
toOp '-' = Sub
toOp '*' = Mul
toOp '/' = Div
toOp _ = error "should be guarded by parser"

-- | Parse an optional sign preceding a value.
sign :: Parses Sign
sign =
  do char '-'; return Negative
    <|> do char '+'; return Positive
    <|> return Positive

signToFunc :: Num a => Sign -> (a -> a)
signToFunc Positive = id
signToFunc Negative = negate

-- | Parses at least 1 digit, as Text.
digits :: Parses Text
digits = T.pack <$> many1 digit

-- | Get the number of significant figures for a
-- non-negative integer if it was typed as text.
numSigFigsNNIntTextual :: Text -> Integer
numSigFigsNNIntTextual t =
  let residue = T.dropAround (== '0') t
   in toInteger $ if T.null residue then 1 else T.length residue

-- | Get the number of significant figures for a
-- non-negative float if it was typed as text.
numSigFigsNNFltTextual :: Text -> Integer
numSigFigsNNFltTextual t =
  let residue = T.dropWhile (== '0') . T.filter (/= '.') $ t
   in toInteger $ if T.null residue then T.count "0" t else T.length residue

-- | Parse an integer which may have a sign.
integer :: Parses Term
integer = do
  s <- sign
  digs <- digits
  return . Measured (numSigFigsNNIntTextual digs) . signToFunc s . BD.fromString . T.unpack $ digs

-- | Parse a float which may have a sign.
float :: Parses Term
float = do
  s <- sign
  ldigs <- option "" digits
  char '.'
  rdigs <- option "" digits
  when (T.null ldigs && T.null rdigs) (unexpected "dot without other digits")
  let flt = ldigs <> "." <> rdigs
  return . Measured (numSigFigsNNFltTextual flt) . signToFunc s . BD.fromString . T.unpack $ flt

sciNotation :: Parses Term
sciNotation = do
  Measured sf coef@(BigDecimal coefValue coefScale) <- try float <|> try integer
  char 'e'
  Measured _ (BigDecimal exp _) <- integer
  return $ Measured sf $ BD.nf $ coef * 10 ^^ exp

integerConstant :: Parses Term
integerConstant = do
  Measured _ (BigDecimal v _) <- integer
  char 'c'
  return . Constant $ v % 1

floatConstant :: Parses Term
floatConstant = do
  Measured _ (BigDecimal v s) <- float
  char 'c'
  return . Constant $ v % (10 ^ s)

sciNotationConstant :: Parses Term
sciNotationConstant = do
  Measured _ (BigDecimal v s) <- sciNotation
  char 'c'
  return . Constant $ v % (10 ^ s)

leaf :: Parses Expr
leaf = do
  l <- choice $ try <$> [sciNotationConstant, floatConstant, integerConstant, sciNotation, float, integer]
  return $ Leaf l

factor :: Parses Expr
factor = do
  operand `chainl1` operator
  where
    operand = choice [try $ betweenParens expr, try leaf, function] <* spaces
    operator = do
      try $ string "**" <* spaces
      pure Exp

term :: Parses Expr
term = do
  factor `chainr1` operator
  where
    operator = do
      op <- toOp <$> oneOf "*/" <* spaces
      pure $
        \a -> \case
          Prec2 xs -> Prec2 $ (op, a) : xs
          o -> Prec2 [(Mul, a), (op, o)]

expr :: Parses Expr
expr = do
  term `chainr1` operator
  where
    operator = do
      op <- toOp <$> oneOf "+-" <* spaces
      pure $
        \a -> \case
          Prec1 xs -> Prec1 $ (op, a) : xs
          o -> Prec1 [(Add, a), (op, o)]

-- ❯ parseEval "344 ** 2 ** 4"
-- Right (Measured {numSigFigs = 3, value = 194000000000000000000})
-- ❯ (344 ^ 2) ^ 4
-- 196095460708571938816

-- | A list of all the functions available.
funcMap :: [(Function, Text)]
funcMap =
  [ (Log10, "log"),
    (Antilog10, "exp")
  ]

genFuncParsers :: [Parses Expr]
genFuncParsers = do
  (f, t) <- funcMap
  pure $ do
    string $ T.unpack t
    char '('
    e <- expr
    char ')'
    pure $ Apply f e

-- | Parses a function application.
function :: Parses Expr
function = choice genFuncParsers

betweenParens :: Parses a -> Parses a
betweenParens p = char '(' *> spaces *> p <* spaces <* char ')'
