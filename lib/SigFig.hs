{-# LANGUAGE OverloadedStrings #-}

module SigFig
  ( module SigFig,
    module Data.BigDecimal,
  )
where

import Control.Monad
import Data.BigDecimal (BigDecimal)
import qualified Data.BigDecimal as BD
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import Data.Tuple.Extra (second)
import Text.Parsec
import Text.Parsec.Char

type Parses = Parsec Text ()

data SFTerm = SFTerm {numSigFigs :: Integer, value :: BigDecimal}
  deriving (Show, Eq)

niceShow :: SFTerm -> Text
niceShow (SFTerm sf bd) = T.pack $ BD.toString bd ++ " (" ++ show sf ++ " s.f.)"

data Sign = Positive | Negative
  deriving (Show, Eq)

data Op
  = Add
  | Sub
  | Mul
  | Div
  | Exp
  deriving (Show, Eq)

toOp :: Char -> Op
toOp '+' = Add
toOp '-' = Sub
toOp '*' = Mul
toOp '/' = Div
toOp '^' = Exp
toOp _ = error "should be guarded by parser"

data SFTree
  = SFLeaf SFTerm
  | SFPrec1 [(Op, SFTree)]
  | SFPrec2 [(Op, SFTree)]
  deriving (Show)

children :: SFTree -> [(Op, SFTree)]
children (SFPrec1 xs) = xs
children (SFPrec2 xs) = xs
children _ = []

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

integerLike :: Parses SFTerm
integerLike = do
  s <- sign
  digs <- digits
  return . SFTerm (numSigFigsNNIntTextual digs) . signF s . BD.fromString . T.unpack $ digs

floatLike :: Parses SFTerm
floatLike = do
  s <- sign
  ldigs <- option "" digits
  char '.'
  rdigs <- option "" digits
  when (T.null ldigs && T.null rdigs) (unexpected "just a dot")
  let flt = ldigs <> "." <> rdigs
  return . SFTerm (numSigFigsNNFltTextual flt) . signF s . BD.fromString . T.unpack $ flt

sciNotationLike :: Parses SFTerm
sciNotationLike = do
  SFTerm sf coef@(BD.BigDecimal coefValue coefScale) <- try floatLike <|> try integerLike
  char 'e'
  SFTerm _ (BD.BigDecimal exp _) <- integerLike
  return $ SFTerm sf $ BD.nf $ coef * 10 ^^ exp

leaf :: Parses SFTree
leaf = do
  l <- try sciNotationLike <|> try floatLike <|> try integerLike
  return $ SFLeaf l

expr :: Parses SFTree
expr =
  try prec2Chain
    <|> try prec1Chain
    <|> try leaf
    <|> try (btwnParens expr)

fullExpr :: Parses SFTree
fullExpr =
  choice $
    try . (<* eof)
      <$> [ btwnParens expr,
            prec2Chain,
            prec1Chain,
            leaf
          ]

-- addition and subtraction. chains are necessary because sigfig-simplification
-- only occurs on completion of evaluation of such a chain
prec1Chain :: Parses SFTree
prec1Chain =
  do
    term <- try (btwnParens expr) <|> try prec2Chain <|> leaf
    spaces
    op <- oneOf "+-"
    spaces
    term' <- try (btwnParens expr) <|> try prec2Chain <|> leaf
    spaces
    rest [(toOp op, term'), (Add, term)]
  where
    rest terms =
      do
        op <- oneOf "+-"
        spaces
        term' <- try (btwnParens expr) <|> try prec2Chain <|> leaf
        spaces
        rest ((toOp op, term') : terms)
        <|> return (SFPrec1 (reverse terms))

prec2Chain :: Parses SFTree
prec2Chain =
  do
    term <- try (btwnParens expr) <|> leaf
    spaces
    op <- oneOf "*/"
    spaces
    term' <- try (btwnParens expr) <|> leaf
    spaces
    rest [(toOp op, term'), (Mul, term)]
  where
    rest terms =
      do
        op <- oneOf "*/"
        spaces
        term' <- try (btwnParens expr) <|> leaf
        spaces
        rest ((toOp op, term') : terms)
        <|> return (SFPrec2 (reverse terms))

btwnParens :: Parses a -> Parses a
btwnParens p = char '(' *> spaces *> p <* spaces <* char ')'

-- positive integer means to the right of decimal place, negative means to the left
roundToPlace :: BigDecimal -> Integer -> BigDecimal
roundToPlace bd@(BD.BigDecimal v s) dp
  | dp > 0 = BD.roundBD bd $ BD.halfUp dp
  | otherwise =
    let bd' = BD.BigDecimal v (s - dp)
     in BD.roundBD bd' (BD.halfUp 0) * 10 ^ (- dp)

evaluate :: SFTree -> SFTerm
evaluate t = case t of
  (SFLeaf a) -> a
  (SFPrec1 xs) -> case xs of
    [] -> error "should not happen"
    [(_, SFLeaf a)] -> a
    xs ->
      let evaledSubs = evaluateSubtrees xs
          s = computeUnconstrained evaledSubs prec1Id
          minDP = minimum . map (significantDecPlaces . snd) $ evaledSubs
          res = BD.nf $ roundToPlace s minDP
       in SFTerm (BD.precision res - BD.getScale res + minDP) res
  (SFPrec2 xs) -> case xs of
    [] -> error "should not happen"
    [(_, SFLeaf a)] -> a
    xs ->
      let evaledSubs = evaluateSubtrees xs
          s = computeUnconstrained evaledSubs prec2Id
          minSF = minimum . map (numSigFigs . snd) $ evaledSubs
       in forceSF minSF s
  where
    evaluateSubtrees = map (second evaluate)
    prec1Id = 0
    prec2Id = 1
    doOp Add a b = a + b
    doOp Sub a b = a - b
    doOp Mul a b = a * b
    doOp Div a b = BD.divide (a, b) (BD.HALF_UP, Nothing)
    doOp _ a b = error "should not happen"
    computeUnconstrained terms identity = foldl' (\acc (op, SFTerm _ v) -> doOp op acc v) identity terms
    delta sf bd =
      let v' = BD.nf bd
          dec = BD.getScale v'
          nd = BD.precision v'
       in sf + dec - nd
    significantDecPlaces (SFTerm sf v) = delta sf v
    forceSF sf' bd =
      let bd' = BD.nf bd
          dec = BD.getScale bd'
          nd = BD.precision bd'
          delta = sf' + dec - nd
       in SFTerm sf' (roundToPlace bd' delta)

maybeParse :: Text -> Maybe SFTerm
maybeParse e = case parse fullExpr "" e of
  Right m -> Just $ evaluate m
  Left _ -> Nothing

processExpression :: Text -> Text
processExpression e = case parse fullExpr "" e of
  Right m -> niceShow $ evaluate m
  Left n -> "Error: " <> T.pack (show n)
