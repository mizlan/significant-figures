{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.BigDecimal (BigDecimal)
import qualified Data.BigDecimal as BD
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Text.IO as T
import Data.Tuple.Extra (second)
import Debug.Trace
import Text.Parsec
import Text.Parsec.Char

type Parses = Parsec Text ()

data SFTerm = SFTerm {numSigFigs :: Integer, value :: BigDecimal}
  deriving (Show)

niceShow :: SFTerm -> Text
niceShow (SFTerm sf bd) = T.pack $ BD.toString bd ++ " (" ++ show sf ++ " sig. figs.)"

data Sign = Positive | Negative
  deriving (Show)

data Op
  = Add
  | Sub
  | Mul
  | Div
  | Exp
  deriving (Show)

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
  SFTerm sf coef@(BD.BigDecimal coefValue coefScale) <- try floatLike <|> integerLike
  char 'e'
  SFTerm _ (BD.BigDecimal exp _) <- integerLike
  return $ SFTerm sf $ BD.nf $ coef * 10 ^^ exp

leaf :: Parses SFTree
leaf = do
  l <- try sciNotationLike <|> try floatLike <|> try integerLike
  return $ SFLeaf l

-- addition and subtraction. chains are necessary because sigfig-simplification
-- only occurs on completion of evaluation of such a chain
prec1Chain :: Parses SFTree
prec1Chain = do term <- prec2Chain; spaces; rest [(Add, term)]
  where
    rest :: [(Op, SFTree)] -> Parses SFTree
    rest terms =
      do
        op <- oneOf "+-"
        spaces
        term' <- prec2Chain
        spaces
        rest ((toOp op, term') : terms)
        <|> return (SFPrec1 (reverse terms))

prec2Chain :: Parses SFTree
prec2Chain = do term <- btwnParens prec1Chain <|> leaf; spaces; rest [(Mul, term)]
  where
    rest :: [(Op, SFTree)] -> Parses SFTree
    rest terms =
      do
        op <- oneOf "*/"
        spaces
        term' <- btwnParens prec1Chain <|> leaf
        spaces
        rest ((toOp op, term') : terms)
        <|> return (SFPrec2 (reverse terms))

btwnParens :: Parses a -> Parses a
btwnParens = between (char '(') (char ')')

-- positive integer means to the right of decimal place, negative means to the left
roundToPlace :: BigDecimal -> Integer -> BigDecimal
roundToPlace bd@(BD.BigDecimal v s) dp
 | dp > 0 = BD.roundBD bd $ BD.halfUp dp
 | otherwise = let bd' = BD.BigDecimal v (s - dp)
                in BD.roundBD bd' (BD.halfUp 0) * 10 ^ (-dp)

-- The below is not pretty
--
-- 123128318526389823467529874356923 14
-- ┗━━━━n━━━━━━━━━━━━━━━━━━━━━━━━━━┛
--           ┗━━━━d━━━━━━━━━━━━━━━━┛
-- ┗━━━━s━━━━━━━━━━━━━━━━┛
--           ┗━━━━!━━━━━━┛ take minimum
evaluate :: SFTree -> SFTerm
evaluate (SFLeaf a) = a
evaluate (SFPrec1 xs) = case xs of
  [] -> error "should not happen"
  [(_, SFLeaf a)] -> a
  xs ->
    let evaledSubs = map (second evaluate) xs
        s = foldl' (\acc (op, SFTerm _ v) -> doOp op acc v) 0 evaledSubs
        minDP = minimum . map (significantDecPlaces . snd) $ evaledSubs
        res = BD.nf $ roundToPlace s minDP
     in SFTerm (BD.precision res - BD.getScale res + minDP) res
    where
      significantDecPlaces (SFTerm sf v) =
        let v' = BD.nf v
            dec = BD.getScale v'
            nd = BD.precision v'
         in sf + dec - nd
      doOp Add a b = a + b
      doOp Sub a b = a - b
      doOp _ a b = error "should not happen"
evaluate (SFPrec2 xs) = case xs of
  [] -> error "should not happen"
  [(_, SFLeaf a)] -> a
  xs ->
    let evaledSubs = map (second evaluate) xs
        s = foldl' (\acc (op, SFTerm _ v) -> doOp op acc v) 1 evaledSubs
        minSF = minimum . map (numSigFigs . snd) $ evaledSubs
     in forceSF minSF s
    where
      doOp Mul a b = a * b
      doOp Div a b = BD.divide (a, b) (BD.HALF_UP, Nothing)
      doOp _ a b = error "should not happen"
      forceSF sf' bd =
        let bd' = BD.nf bd
            dec = BD.getScale bd'
            nd = BD.precision bd'
            delta = sf' + dec - nd
         in SFTerm sf' (roundToPlace bd' delta)

-- can be
-- 2.5e7
-- 400e+25
-- -2.e-4
-- 0000.003
-- not accounted for: -.7

main :: IO ()
main = T.putStrLn $ case parse prec2Chain "" "(3.5 + 2.0) * 2.0" of
  Right m -> niceShow $ evaluate m
  _ -> "fail"
