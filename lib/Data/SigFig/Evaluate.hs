{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.SigFig.Evaluate (evaluate) where

import Data.BigDecimal (BigDecimal (..))
import Data.BigDecimal qualified as BD
import Data.Foldable (foldl')
import Data.SigFig.Types
import Data.SigFig.Util
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tuple.Extra (second)
import Text.Printf (printf)

evaluate :: Expr -> Either Text Term
evaluate (Leaf a) = Right a
evaluate (Prec1 xs) = case xs of
  [] -> Left "should not happen"
  [(_, Leaf a)] -> Right a
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
  [(_, Leaf a)] -> Right a
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
  case res of
    (Measured sf bd) -> Right $ forceSF sf (bd ^ e)
    (Constant a) -> Right . Constant $ a ^ e
evaluate (Apply Log10 e) = do
  res <- evaluate e
  case res of
    v@(Measured sf bd) ->
      if bd < 0
        then Left $ "cannot evaluate log(" <> display v <> "), argument is negative"
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
    v@(Measured sf bd) ->
      let dp = rightmostSignificantPlace sf bd
       in if dp >= 0
            then Left $ display v <> " has 0 significant decimal places so exp(" <> display v <> ") is undefined"
            else
              Right . forceSF (negate dp) . BD.fromString
                . printf "%f"
                $ (10 :: Float) ** realToFrac bd
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

forceSF :: Integer -> BigDecimal -> Term
forceSF sf' bd = Measured sf' . roundToPlace bd . rightmostSignificantPlace sf' $ bd

evaluateSubtrees :: [(a, Expr)] -> Either Text [(a, Term)]
evaluateSubtrees xs = traverse sequenceA $ second evaluate <$> xs
