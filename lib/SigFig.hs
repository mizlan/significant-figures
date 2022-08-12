{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module SigFig where

import Control.Monad
import Data.BigDecimal (BigDecimal (..))
import qualified Data.BigDecimal as BD
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import Data.Tuple.Extra (second)
import GHC.Real (Ratio ((:%)), (%))
import Text.Parsec
import Text.Parsec.Char

type Parses = Parsec Text ()

data SFTerm
  = SFMeasured {numSigFigs :: Integer, value :: BigDecimal}
  | SFConstant Rational
  deriving (Show, Eq)

isMeasured (SFMeasured _ _) = True
isMeasured (SFConstant _) = False

niceShow :: SFTerm -> Text
niceShow (SFMeasured sf bd) = T.pack $ BD.toString bd ++ " (" ++ show sf ++ " s.f.)"
niceShow (SFConstant v@(a :% b)) =
  T.pack $
    if isTerminating b
      then (BD.toString . BD.nf . fromRational $ v) ++ " (const)"
      else show a ++ "/" ++ show b ++ " (non-terminating const)"
  where
    isTerminating = (== 1) . extRem 5 . extRem 2
      where
        extRem d n = case n `quotRem` d of
          (q, 0) -> extRem d q
          _ -> n
data Sign = Positive | Negative
  deriving (Show, Eq)

data Op
  = Add
  | Sub
  | Mul
  | Div
  deriving (Show, Eq)

toOp :: Char -> Op
toOp '+' = Add
toOp '-' = Sub
toOp '*' = Mul
toOp '/' = Div
toOp _ = error "should be guarded by parser"

data SFTree
  = SFLeaf SFTerm
  | SFPrec1 [(Op, SFTree)]
  | SFPrec2 [(Op, SFTree)]
  | SFExp SFTree Integer
  deriving (Show)

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
  return . SFMeasured (numSigFigsNNIntTextual digs) . signF s . BD.fromString . T.unpack $ digs

floatLike :: Parses SFTerm
floatLike = do
  s <- sign
  ldigs <- option "" digits
  char '.'
  rdigs <- option "" digits
  when (T.null ldigs && T.null rdigs) (unexpected "just a dot")
  let flt = ldigs <> "." <> rdigs
  return . SFMeasured (numSigFigsNNFltTextual flt) . signF s . BD.fromString . T.unpack $ flt

sciNotationLike :: Parses SFTerm
sciNotationLike = do
  SFMeasured sf coef@(BigDecimal coefValue coefScale) <- try floatLike <|> try integerLike
  char 'e'
  SFMeasured _ (BigDecimal exp _) <- integerLike
  return $ SFMeasured sf $ BD.nf $ coef * 10 ^^ exp

integerConstant :: Parses SFTerm
integerConstant = do
  SFMeasured _ (BigDecimal v _) <- integerLike
  char 'c'
  return . SFConstant $ v % 1

floatConstant :: Parses SFTerm
floatConstant = do
  SFMeasured _ (BigDecimal v s) <- floatLike
  char 'c'
  return . SFConstant $ v % (10 ^ s)

sciNotationConstant :: Parses SFTerm
sciNotationConstant = do
  SFMeasured _ (BigDecimal v s) <- sciNotationLike
  char 'c'
  return . SFConstant $ v % (10 ^ s)

leaf :: Parses SFTree
leaf = do
  l <- choice $ try <$> [sciNotationConstant, floatConstant, integerConstant, sciNotationLike, floatLike, integerLike]
  return $ SFLeaf l

exponentE :: Parses SFTree
exponentE = do
  e <- try do
    k <- try (btwnParens expr) <|> try leaf
    spaces
    string "**"
    spaces
    return k
  i <- toInteger . BD.getValue . BD.nf . value <$> try integerLike
  when (i < 0) $ unexpected "negative exponent"
  return $ SFExp e i

expr :: Parses SFTree
expr =
  try prec1Chain
    <|> try prec2Chain
    <|> exponentE
    <|> try (btwnParens expr)
    <|> try leaf

fullExpr :: Parses SFTree
fullExpr =
  choice
    [ try prec1Chain <* eof,
      try prec2Chain <* eof,
      exponentE <* eof,
      try (btwnParens expr) <* eof,
      leaf <* eof
    ]

-- generate chains: necessary because sigfig-simplification
-- only occurs on completion of evaluation of such a chain
precChain :: [Parses SFTree] -> Parses Char -> ([(Op, SFTree)] -> SFTree) -> Op -> Parses SFTree
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

prec1Chain :: Parses SFTree
prec1Chain =
  precChain
    [try prec2Chain, exponentE, try $ btwnParens expr, leaf]
    (oneOf "+-")
    SFPrec1
    Add

prec2Chain :: Parses SFTree
prec2Chain =
  precChain
    [exponentE, try $ btwnParens expr, leaf]
    (oneOf "*/")
    SFPrec2
    Mul

btwnParens :: Parses a -> Parses a
btwnParens p = char '(' *> spaces *> p <* spaces <* char ')'

-- positive integer means to the right of decimal place, negative means to the left
roundToPlace :: BigDecimal -> Integer -> BigDecimal
roundToPlace bd@(BigDecimal v s) dp
  | dp > 0 = BD.roundBD bd $ BD.halfUp dp
  | otherwise =
    let bd' = BigDecimal v (s - dp)
     in BD.roundBD bd' (BD.halfUp 0) * 10 ^ (- dp)

evaluate :: SFTree -> SFTerm
evaluate t = case t of
  (SFLeaf a) -> a
  (SFPrec1 xs) -> case xs of
    [] -> error "should not happen"
    [(_, SFLeaf a)] -> a
    xs ->
      let evaledSubs = evaluateSubtrees xs
          measured = filter (isMeasured . snd) evaledSubs
       in if null measured
            then SFConstant $ computeUnconstrained evaledSubs prec1Id
            else
              let s = computeUnconstrained evaledSubs prec1Id
                  minDP = minimum $ [significantDecPlaces sf bd | (_, SFMeasured sf bd) <- measured]
                  res = BD.nf $ roundToPlace (fromRational s) minDP
               in SFMeasured (BD.precision res - BD.getScale res + minDP) res
  (SFPrec2 xs) -> case xs of
    [] -> error "should not happen"
    [(_, SFLeaf a)] -> a
    xs ->
      let evaledSubs = evaluateSubtrees xs
          measured = filter (isMeasured . snd) evaledSubs
       in if null measured
            then SFConstant $ computeUnconstrained evaledSubs prec2Id
            else
              let s = computeUnconstrained evaledSubs prec2Id
                  minSF = minimum . map (numSigFigs . snd) $ measured
               in forceSF minSF (fromRational s)
  (SFExp b e) -> case evaluate b of
    (SFMeasured sf bd) -> forceSF sf (bd ^ e)
    (SFConstant a) -> SFConstant $ a ^ e
  where
    evaluateSubtrees = map (second evaluate)
    prec1Id = 0
    prec2Id = 1
    computeUnconstrained :: [(Op, SFTerm)] -> Rational -> Rational
    computeUnconstrained terms identity =
      foldl' (uncurry . flip doOpConstant) identity (second extractRat <$> terms)
      where
        extractRat (SFMeasured _ v) = toRational v
        extractRat (SFConstant v) = v
    doOpConstant :: Op -> Rational -> Rational -> Rational
    doOpConstant Add a b = a + b
    doOpConstant Sub a b = a - b
    doOpConstant Mul a b = a * b
    doOpConstant Div a b = a / b
    significantDecPlaces sf bd =
      let v' = BD.nf bd
          dec = BD.getScale v'
          nd = BD.precision v'
       in sf + dec - nd
    forceSF sf' bd = SFMeasured sf' $ roundToPlace bd $ significantDecPlaces sf' bd

textify :: Either ParseError a -> Either Text a
textify (Right m) = Right m
textify (Left e) = Left . T.pack . show $ e

parseFullExpr :: Text -> Either Text SFTree
parseFullExpr e = textify $ parse fullExpr "" e

maybeParseEval :: Text -> Maybe SFTerm
maybeParseEval e = case parse fullExpr "" e of
  Right m -> Just $ evaluate m
  Left _ -> Nothing

processExpression :: Text -> Text
processExpression e = case parse fullExpr "" e of
  Right m -> niceShow $ evaluate m
  Left n -> "Error: " <> T.pack (show n)
