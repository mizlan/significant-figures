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
import Text.Printf (printf)

type Parses = Parsec Text ()

data SFTerm
  = SFMeasured {numSigFigs :: Integer, value :: BigDecimal}
  | SFConstant Rational
  deriving (Show, Eq)

isMeasured (SFMeasured _ _) = True
isMeasured (SFConstant _) = False

-- >>> niceShow (SFMeasured 3 (BigDecimal 200 0))
-- "200. (3 s.f.)"
-- >>> niceShow (SFMeasured 5 (BigDecimal 21 1))
-- "2.10 (3 s.f.)"
niceShow :: SFTerm -> Text
niceShow (SFMeasured sf bd@(BigDecimal v s)) = format (BD.nf bd) <> " (" <> ssf <> " s.f.)"
  where
    ssf = T.pack $ show sf
    format term =
      let sdp = significantDecPlaces sf bd
       in if sdp > 0
            then
              T.pack (BD.toString bd)
                <> if sdp > s
                  then
                    if s == 0
                      then "." <> T.replicate (fromIntegral (sdp - s)) "0"
                      else T.replicate (fromIntegral (sdp - s)) "0"
                  else ""
            else
              let p = BD.precision bd
               in if sf == p && v `mod` 10 == 0
                    then T.pack $ BD.toString bd <> "."
                    else
                      if sf < p
                        then
                          let coef = BD.nf . BigDecimal v $ s + (p - 1)
                           in T.pack $ BD.toString coef <> " x 10^" <> show (p - 1)
                        else T.pack $ BD.toString bd
niceShow (SFConstant v@(a :% b)) =
  T.pack $
    if isTerminating b
      then (BD.toString . BD.nf . fromRational $ v) ++ " (const)"
      else show a ++ "/" ++ show b ++ " (non-terminating const)"
  where
    isTerminating = (== 1) . stripFactor 5 . stripFactor 2
      where
        stripFactor d n = case n `quotRem` d of
          (q, 0) -> stripFactor d q
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

data Function = Log10 | Antilog10
  deriving (Show)

data SFTree
  = SFLeaf SFTerm
  | SFPrec1 [(Op, SFTree)]
  | SFPrec2 [(Op, SFTree)]
  | SFExp SFTree Integer
  | SFFunction Function SFTree
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

funcMap :: [(Function, Text)]
funcMap =
  [ (Log10, "log"),
    (Antilog10, "exp")
  ]

genFuncParsers :: [Parses SFTree]
genFuncParsers = do
  (f, t) <- funcMap
  pure $ do
    string $ T.unpack t
    char '('
    e <- expr
    char ')'
    pure $ SFFunction f e

--- >>> funcMap

function :: Parses SFTree
function = choice genFuncParsers

expr :: Parses SFTree
expr =
  try prec1Chain
    <|> try prec2Chain
    <|> exponentE
    <|> try (btwnParens expr)
    <|> try function
    <|> try leaf

fullExpr :: Parses SFTree
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
    [try prec2Chain, exponentE, try $ btwnParens expr, function, leaf]
    (oneOf "+-")
    SFPrec1
    Add

prec2Chain :: Parses SFTree
prec2Chain =
  precChain
    [exponentE, try $ btwnParens expr, function, leaf]
    (oneOf "*/")
    SFPrec2
    Mul

btwnParens :: Parses a -> Parses a
btwnParens p = char '(' *> spaces *> p <* spaces <* char ')'


-- | Round a BigDecimal to a specified decimal place. A positive integer means
-- to the right of decimal place, negative means to the left
--
-- >>> roundToPlace (BigDecimal 421 2) 1
-- BigDecimal 42 1
roundToPlace :: BigDecimal -> Integer -> BigDecimal
roundToPlace bd@(BigDecimal v s) dp
  | dp > 0 = BD.roundBD bd $ BD.halfUp dp
  | otherwise =
    let bd' = BigDecimal v (s - dp)
     in BD.roundBD bd' (BD.halfUp 0) * 10 ^ (- dp)

evaluate :: SFTree -> Either Text SFTerm
evaluate t = case t of
  (SFLeaf a) -> Right a
  (SFPrec1 xs) -> case xs of
    [] -> Left "should not happen"
    [(_, SFLeaf a)] -> Right a
    xs -> do
      evaledSubs <- evaluateSubtrees xs
      let measured = filter (isMeasured . snd) evaledSubs
       in if null measured
            then Right $ SFConstant $ computeUnconstrained evaledSubs prec1Id
            else
              let s = computeUnconstrained evaledSubs prec1Id
                  -- can be negative
                  minDP = minimum $ [significantDecPlaces sf bd | (_, SFMeasured sf bd) <- measured]
               in Right . forceDP minDP $ fromRational s
  (SFPrec2 xs) -> case xs of
    [] -> Left "should not happen"
    [(_, SFLeaf a)] -> Right a
    xs -> do
      evaledSubs <- evaluateSubtrees xs
      let measured = filter (isMeasured . snd) evaledSubs
       in if null measured
            then Right $ SFConstant $ computeUnconstrained evaledSubs prec2Id
            else
              let s = computeUnconstrained evaledSubs prec2Id
                  minSF = minimum . map (numSigFigs . snd) $ measured
               in Right . forceSF minSF $ fromRational s
  (SFExp b e) -> do
    res <- evaluate b
    case res of
      (SFMeasured sf bd) -> Right $ forceSF sf (bd ^ e)
      (SFConstant a) -> Right . SFConstant $ a ^ e
  (SFFunction f e) -> do
    res <- evaluate e
    case f of
      Log10 ->
        case res of
          (SFMeasured sf bd) ->
            Right . forceDP sf . BD.fromString
              . printf "%f"
              . logBase (10 :: Float)
              . fromRational
              . toRational
              $ bd
          (SFConstant a) -> Left "taking the log of a constant is unsupported"
      Antilog10 ->
        case res of
          v@(SFMeasured sf bd) ->
            let dp = significantDecPlaces sf bd
             in if dp <= 0
                  then Left $ niceShow v <> " has 0 significant decimal places so exp(" <> niceShow v <> ") is undefined"
                  else
                    Right . forceSF dp . BD.fromString
                      . printf "%f"
                      $ (10 :: Float) ** fromRational (toRational bd)
          (SFConstant a) -> Left "taking the antilog of a constant is unsupported"
  where
    evaluateSubtrees :: [(a, SFTree)] -> Either Text [(a, SFTerm)]
    evaluateSubtrees xs = traverse sequenceA $ second evaluate <$> xs
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
    forceSF sf' bd = SFMeasured sf' $ roundToPlace bd $ significantDecPlaces sf' bd

-- negative return value is allowed and meaningful
-- >>> significantDecPlaces 1 (BigDecimal 20 0)
-- -1
significantDecPlaces sf bd =
  let v' = BD.nf bd
      dec = BD.getScale v'
      nd = BD.precision v'
   in sf + dec - nd

forceDP :: Integer -> BigDecimal -> SFTerm
forceDP dp bd =
  let res = BD.nf $ roundToPlace bd dp
   in SFMeasured (BD.precision res - BD.getScale res + dp) res

textify :: Either ParseError a -> Either Text a
textify (Right m) = Right m
textify (Left e) = Left . T.pack . show $ e

parseEval :: Text -> Either Text SFTerm
parseEval e = textify (parse fullExpr "" e) >>= evaluate

processExpression :: Text -> Text
processExpression e = either ("Error:" <>) niceShow $ parseEval e
