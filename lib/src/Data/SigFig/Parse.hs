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
import Data.Ratio (denominator, numerator)
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
parse = textify . P.parse fullExpr ""
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

exponent :: Parses Expr
exponent = do
  (base, e) <- try do
    base <- operand
    op <- operator
    e <- operand
    pure (base, e)
  e' <- exprNNInt e
  exps <- many do
    op <- operator
    term' <- operand
    exprNNInt term'
  pure $ foldr' (flip Exp) base (e' : exps)
  where
    operand = choice [try $ betweenParens expr <|> try leaf] <* spaces
    operator = string "**" <* spaces
    toNNInt (Measured sf (BigDecimal v s)) =
      if s == 0 && v >= 0 then Just v else Nothing
    toNNInt (Constant a) =
      if denominator a == 1 && a >= 0 then Just (numerator a) else Nothing
    exprNNInt e = case e of
      Leaf k | Just n <- toNNInt k -> pure n
      _ -> unexpected "non-integer exponent"

-- exponent :: Parses Expr
-- exponent = do
--   e <- try do
--     k <- try (betweenParens expr) <|> try leaf
--     spaces
--     string "**"
--     spaces
--     return k
--   i <- toInteger . BD.value . BD.nf . value <$> try integer
--   when (i < 0) $ unexpected "negative exponent"
--   return $ Exp e i

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

-- | Parses any expression.
expr :: Parses Expr
expr =
  try prec1Chain
    <|> try prec2Chain
    <|> exponent
    <|> try (betweenParens expr)
    <|> try function
    <|> try leaf

-- | Parses a full expression.
fullExpr :: Parses Expr
fullExpr =
  choice
    [ try prec1Chain <* eof,
      try prec2Chain <* eof,
      exponent <* eof,
      try (betweenParens expr) <* eof,
      try function <* eof,
      leaf <* eof
    ]

-- Generate a chain parser: necessary because sigfig-simplification
-- only occurs on completion of evaluation of such a chain.
precChain :: [Parses Expr] -> Parses Char -> ([(Op, Expr)] -> Expr) -> Op -> Parses Expr
precChain validOperands validOperator constructor idOp =
  do
    term <- operand
    op <- operator
    term' <- operand
    rest [(toOp op, term'), (idOp, term)]
  where
    operand = choice validOperands <* spaces
    operator = validOperator <* spaces
    rest terms =
      do
        op <- operator
        term' <- operand
        rest ((toOp op, term') : terms)
        <|> (pure . constructor $ reverse terms)

-- | Parse a precendence-2 chain (of both addition or subtraction)
prec1Chain :: Parses Expr
prec1Chain =
  precChain
    [try prec2Chain, exponent, try $ betweenParens expr, function, leaf]
    (oneOf "+-")
    Prec1
    Add

-- | Parse a precendence-2 chain (of both multiplication or division)
prec2Chain :: Parses Expr
prec2Chain =
  precChain
    [exponent, try $ betweenParens expr, function, leaf]
    (oneOf "*/")
    Prec2
    Mul

betweenParens :: Parses a -> Parses a
betweenParens p = char '(' *> spaces *> p <* spaces <* char ')'
