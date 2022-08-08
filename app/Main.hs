{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.BigDecimal (BigDecimal)
import qualified Data.BigDecimal as BD
import Data.List
import Data.Text (Text)
import Data.Tuple.Extra (second)
import qualified Data.Text as T
import Data.Text.Read as T
import Text.Parsec
import Text.Parsec.Char
import Debug.Trace

type Parses = Parsec Text ()

data SFTerm = SFTerm {numSigFigs :: Integer, value :: BigDecimal}
  deriving (Show)

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
numSigFigsNNIntTextual = toInteger . T.length . T.dropAround (== '0')

numSigFigsNNFltTextual :: Text -> Integer
numSigFigsNNFltTextual = toInteger . T.length . T.dropWhile (== '0') . T.filter (/= '.')

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
        term' <- prec2Chain
        spaces
        rest ((toOp op, term') : terms)
        <|> return (SFPrec2 (reverse terms))

btwnParens :: Parses a -> Parses a
btwnParens = between (char '(') (char ')')

--
-- 123128318526389823467529874356923 14
-- ┗━━━━n━━━━━━━━━━━━━━━━━━━━━━━━━━┛
--           ┗━━━━d━━━━━━━━━━━━━━━━┛
-- ┗━━━━s━━━━━━━━━━━━━━━━┛
--           ┗━━━━!━━━━━━┛ take minimum
--
-- 422 0
-- -> 400
-- 4.22
evalPrec1 :: [(Op, SFTree)] -> SFTerm
evalPrec1 [] = error "should not happen"
evalPrec1 [(_, SFLeaf a)] = a
evalPrec1 xs =
  let evaledSubs = map (second (evalPrec2 . children)) xs
      s = foldl' (\acc (op, SFTerm _ v) -> doOp op acc v) 0 evaledSubs
      minDP = traceShowId (minimum . map (significantDecPlaces . snd) $ evaledSubs)
      res = roundDP minDP s
  in SFTerm (BD.precision res - BD.getScale res) res
    where
      significantDecPlaces (SFTerm sf v) =
        let v' = BD.nf v
            dec = BD.getScale v'
            nd = BD.precision v'
        in sf + dec - nd
      doOp Add a b = a + b
      doOp Sub a b = a - b
      doOp _ a b = error "should not happen"
      roundDP minDP s
       | minDP > 0 = BD.roundBD s $ BD.halfUp minDP
       | otherwise = true

evalPrec2 :: [(Op, SFTree)] -> SFTerm
evalPrec2 [] = error "should not happen"
evalPrec2 [(_, SFLeaf a)] = a
evalPrec2 _ = undefined

-- can be
-- 2.5e7
-- 400e+25
-- -2.e-4
-- 0000.003
-- not accounted for: -.7

main :: IO ()
main = print $ case parse prec1Chain "" "-.7 + 4.2e4" of
                 Right (SFPrec1 m) -> show $ evalPrec1 m
                 _ -> "fail"
