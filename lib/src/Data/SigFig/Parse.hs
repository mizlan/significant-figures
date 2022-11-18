{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
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
parse = textify . P.parse (expr <* eof) ""
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
  do char '-'; pure Negative
    <|> do char '+'; pure Positive
    <|> pure Positive

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
  pure . Measured (numSigFigsNNIntTextual digs) . signToFunc s . BD.fromString . T.unpack $ digs

-- | Parse a float which may have a sign.
float :: Parses Term
float = do
  s <- sign
  ldigs <- option "" digits
  char '.'
  rdigs <- option "" digits
  when (T.null ldigs && T.null rdigs) (unexpected "dot without other digits")
  let flt = ldigs <> "." <> rdigs
  pure . Measured (numSigFigsNNFltTextual flt) . signToFunc s . BD.fromString . T.unpack $ flt

sciNotation :: Parses Term
sciNotation = do
  Measured sf coef@(BigDecimal coefValue coefScale) <- try float <|> try integer
  char 'e'
  Measured _ (BigDecimal exp _) <- integer
  pure $ Measured sf $ BD.nf $ coef * 10 ^^ exp

integerConstant :: Parses Term
integerConstant = do
  Measured _ (BigDecimal v _) <- integer
  char 'c'
  pure . Constant $ v % 1

floatConstant :: Parses Term
floatConstant = do
  Measured _ (BigDecimal v s) <- float
  char 'c'
  pure . Constant $ v % (10 ^ s)

sciNotationConstant :: Parses Term
sciNotationConstant = do
  Measured _ (BigDecimal v s) <- sciNotation
  char 'c'
  pure . Constant $ v % (10 ^ s)

literal :: Parses Expr
literal = do
  l <- choice $ try <$> [sciNotationConstant, floatConstant, integerConstant, sciNotation, float, integer]
  pure $ Literal l

factor :: Parses Expr
factor = do
  operand `chainl1` operator
  where
    operand = choice [try $ betweenParens expr, try literal, function] <* spaces
    operator = Exp <$ try (string "**" <* spaces)

term :: Parses Expr
term = do
  factor `chainl1'` (op, Mul, Prec2)
  where
    op = toOp <$> oneOf "*/" <* spaces

expr :: Parses Expr
expr = do
  term `chainl1'` (op, Add, Prec1)
  where
    op = toOp <$> oneOf "+-" <* spaces

chainl1' :: Parses Expr -> (Parses Op, Op, [(Op, Expr)] -> Expr) -> Parses Expr
{-# INLINEABLE chainl1' #-}
chainl1' p (o, i, c) = do x <- p; rest [(i, x)]
  where
    rest x =
      do
        op <- o
        y <- p
        rest $ (op, y) : x
        <|> pure (if length x > 1 then c (reverse x) else snd $ head x)

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
