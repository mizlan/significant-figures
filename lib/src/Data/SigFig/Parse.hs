{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.SigFig.Parse where

import Control.Monad (when)
import Data.BigDecimal (BigDecimal (BigDecimal))
import Data.BigDecimal qualified as BD
import Data.SigFig.Types
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Real (Ratio ((:%)), (%))
import Text.Parsec

type Parses = Parsec Text ()

toOp :: Char -> Op
toOp '+' = Add
toOp '-' = Sub
toOp '*' = Mul
toOp '/' = Div
toOp _ = error "should be guarded by parser"

sign :: Parses Sign
sign =
  do char '-'; return Negative
    <|> do char '+'; return Positive
    <|> return Positive

signF :: Num a => Sign -> (a -> a)
signF Positive = id
signF Negative = negate

digits :: Parses Text
digits = T.pack <$> many1 digit

-- number of sig figs for a non-negative integer if it was typed as text
numSigFigsNNIntTextual :: Text -> Integer
numSigFigsNNIntTextual t =
  let residue = T.dropAround (== '0') t
   in toInteger $ if T.null residue then 1 else T.length residue

numSigFigsNNFltTextual :: Text -> Integer
numSigFigsNNFltTextual t =
  let residue = T.dropWhile (== '0') . T.filter (/= '.') $ t
   in toInteger $ if T.null residue then T.count "0" t else T.length residue

integerLike :: Parses Term
integerLike = do
  s <- sign
  digs <- digits
  return . Measured (numSigFigsNNIntTextual digs) . signF s . BD.fromString . T.unpack $ digs

floatLike :: Parses Term
floatLike = do
  s <- sign
  ldigs <- option "" digits
  char '.'
  rdigs <- option "" digits
  when (T.null ldigs && T.null rdigs) (unexpected "just a dot")
  let flt = ldigs <> "." <> rdigs
  return . Measured (numSigFigsNNFltTextual flt) . signF s . BD.fromString . T.unpack $ flt

sciNotationLike :: Parses Term
sciNotationLike = do
  Measured sf coef@(BigDecimal coefValue coefScale) <- try floatLike <|> try integerLike
  char 'e'
  Measured _ (BigDecimal exp _) <- integerLike
  return $ Measured sf $ BD.nf $ coef * 10 ^^ exp

integerConstant :: Parses Term
integerConstant = do
  Measured _ (BigDecimal v _) <- integerLike
  char 'c'
  return . Constant $ v % 1

floatConstant :: Parses Term
floatConstant = do
  Measured _ (BigDecimal v s) <- floatLike
  char 'c'
  return . Constant $ v % (10 ^ s)

sciNotationConstant :: Parses Term
sciNotationConstant = do
  Measured _ (BigDecimal v s) <- sciNotationLike
  char 'c'
  return . Constant $ v % (10 ^ s)

leaf :: Parses Expr
leaf = do
  l <- choice $ try <$> [sciNotationConstant, floatConstant, integerConstant, sciNotationLike, floatLike, integerLike]
  return $ Leaf l

exponentE :: Parses Expr
exponentE = do
  e <- try do
    k <- try (btwnParens expr) <|> try leaf
    spaces
    string "**"
    spaces
    return k
  i <- toInteger . BD.value . BD.nf . value <$> try integerLike
  when (i < 0) $ unexpected "negative exponent"
  return $ Exp e i

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

function :: Parses Expr
function = choice genFuncParsers

expr :: Parses Expr
expr =
  try prec1Chain
    <|> try prec2Chain
    <|> exponentE
    <|> try (btwnParens expr)
    <|> try function
    <|> try leaf

fullExpr :: Parses Expr
fullExpr =
  choice
    [ try prec1Chain <* eof,
      try prec2Chain <* eof,
      exponentE <* eof,
      try (btwnParens expr) <* eof,
      try function <* eof,
      leaf <* eof
    ]

-- | generate chains: necessary because sigfig-simplification
-- only occurs on completion of evaluation of such a chain
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
        <|> return (constructor (reverse terms))

prec1Chain :: Parses Expr
prec1Chain =
  precChain
    [try prec2Chain, exponentE, try $ btwnParens expr, function, leaf]
    (oneOf "+-")
    Prec1
    Add

prec2Chain :: Parses Expr
prec2Chain =
  precChain
    [exponentE, try $ btwnParens expr, function, leaf]
    (oneOf "*/")
    Prec2
    Mul

btwnParens :: Parses a -> Parses a
btwnParens p = char '(' *> spaces *> p <* spaces <* char ')'
