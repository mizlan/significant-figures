{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.BigDecimal (BigDecimal)
import qualified Data.BigDecimal as BD
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Read as T
import Text.Parsec
import Text.Parsec.Char

type Parses = Parsec Text ()

data SFTerm = SFTerm {numSigFigs :: Int, value :: BigDecimal}
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
numSigFigsNNIntTextual :: Text -> Int
numSigFigsNNIntTextual = T.length . T.dropAround (== '0')

numSigFigsNNFltTextual :: Text -> Int
numSigFigsNNFltTextual = T.length . T.dropWhile (== '0') . T.filter (/= '.')

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

-- todo use choice()
leaf :: Parses SFTree
leaf = do
  l <- try sciNotationLike <|> try floatLike <|> try integerLike
  return $ SFLeaf l

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

evalPrec1 :: [(Op, SFTree)] -> SFTerm

-- can be
-- 2.5e7
-- 400e+25
-- -2.e-4
-- 0000.003
-- not accounted for: -.7

main :: IO ()
main = print $ parse prec1Chain "" "-.7"
