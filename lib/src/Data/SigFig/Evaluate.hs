{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune #-}

-- |
-- This module contains effectively one function of interest, which is 'evaluate'.
-- It takes an 'Expr' and evaluates it, applying the correct significant
-- figure rules. To display the resulting 'Term' that 'evaluate' may return, see 'display'.
module Data.SigFig.Evaluate
  ( evaluate,
    evaluate',
  )
where

import Data.BigDecimal (BigDecimal (..))
import Data.BigDecimal qualified as BD
import Data.Foldable (foldl')
import Data.SigFig.Types
import Data.SigFig.Util
import Data.Text (Text)
import Data.Text qualified as T
import Control.Arrow (second)
import Text.Printf (printf)
import GHC.Real (denominator, numerator)

isMeasured (Measured _ _) = True
isMeasured (Constant _) = False

toNNInt (Measured sf (BigDecimal v s)) =
  if s == 0 && v >= 0 then Just v else Nothing
toNNInt (Constant a) =
  if denominator a == 1 && a >= 0 then Just (numerator a) else Nothing
exprNNInt e
  | Just n <- toNNInt e = pure n
  | otherwise = Left "non-integer exponent"

-- | Like 'evaluate', but assume the result is a valid term and crash otherwise.
evaluate' :: Expr -> Term
evaluate' s = case evaluate s of
  Left e -> error . T.unpack $ "evaluate' crashed because: " <> e
  Right e -> e

-- | Given an expression tree, evaluate it and return either an error or result.
evaluate :: Expr -> Either Text Term
evaluate (Literal a) = Right a
evaluate (Prec1 xs) = case xs of
  [] -> Left "should not happen"
  [(_, Literal a)] -> Right a
  xs -> do
    evaledSubs <- evaluateSubtrees xs
    computed <- computeUnconstrained evaledSubs 0
    let measured = filter (isMeasured . snd) evaledSubs
    if null measured
      then Right $ Constant computed
      else
        let minDP = maximum $ [rightmostSignificantPlace sf bd | (_, Measured sf bd) <- measured]
         in Right . forceDP minDP $ fromRational computed
evaluate (Prec2 xs) = case xs of
  [] -> Left "should not happen"
  [(_, Literal a)] -> Right a
  xs -> do
    evaledSubs <- evaluateSubtrees xs
    computed <- computeUnconstrained evaledSubs 1
    let measured = filter (isMeasured . snd) evaledSubs
    if null measured
      then Right $ Constant computed
      else
        let min = minimum . map (numSigFigs . snd) $ measured
         in Right . forceSF min $ fromRational computed
evaluate (Exp b e) = do
  res <- evaluate b
  exp <- evaluate e >>= exprNNInt
  case res of
    (Measured sf bd) -> Right $ forceSF sf (bd ^ exp)
    (Constant a) -> Right . Constant $ a ^ exp
evaluate (Apply Log10 e) = do
  res <- evaluate e
  case res of
    v@(Measured sf bd) ->
      if bd <= 0
        then do
          Left $ "cannot evaluate log(" <> display v <> "), argument is not positive"
        else
          Right . forceDP (negate sf) . BD.fromString
            . printf "%f"
            . logBase (10 :: Float)
            . realToFrac
            $ bd
    (Constant a) -> Left "taking the log of a constant is unsupported"
evaluate (Apply Antilog10 e) = do
  res <- evaluate e
  case res of
    arg@(Measured sf bd') ->
      let bd@(BigDecimal v s) = BD.nf bd'
          dp = rightmostSignificantPlace sf bd
       in if
              | dp >= 0 -> Left $ display arg <> " has 0 significant decimal places so exp(" <> display arg <> ") is undefined"
              | s == 0 -> Right . forceSF (negate dp) $ BigDecimal (10 ^ v) 1
              | bd > 308 -> Left $ "exp(" <> display arg <> ") is too big! sorry"
              | otherwise ->
                Right . forceSF (negate dp) . BD.fromString
                  . printf "%f"
                  $ (10 :: Double) ** realToFrac bd
    (Constant a) -> Left "taking the antilog of a constant is unsupported"

computeUnconstrained :: [(Op, Term)] -> Rational -> Either Text Rational
computeUnconstrained terms identity =
  foldl' comb (Right identity) (second extractRat <$> terms)
  where
    comb e (o, a) = e >>= flip (doOp o) a
    extractRat (Measured _ v) = toRational v
    extractRat (Constant v) = v

doOp :: Op -> Rational -> Rational -> Either Text Rational
doOp Add a b = Right $ a + b
doOp Sub a b = Right $ a - b
doOp Mul a b = Right $ a * b
doOp Div a b = if b == 0 then Left "division by zero error" else Right $ a / b

evaluateSubtrees :: [(a, Expr)] -> Either Text [(a, Term)]
evaluateSubtrees xs = traverse sequenceA $ second evaluate <$> xs
