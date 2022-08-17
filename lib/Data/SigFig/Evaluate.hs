{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.SigFig.Evaluate where

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
    let measured = filter (isMeasured . snd) evaledSubs
     in if null measured
          then Right $ Constant $ computeUnconstrained evaledSubs 0
          else
            let s = computeUnconstrained evaledSubs 0
                -- can be positive/negative
                minDP = maximum $ [rightmostSignificantPlace sf bd | (_, Measured sf bd) <- measured]
             in Right . forceDP minDP $ fromRational s
evaluate (Prec2 xs) = case xs of
  [] -> Left "should not happen"
  [(_, Leaf a)] -> Right a
  xs -> do
    evaledSubs <- evaluateSubtrees xs
    let measured = filter (isMeasured . snd) evaledSubs
     in if null measured
          then Right $ Constant $ computeUnconstrained evaledSubs 1
          else
            let s = computeUnconstrained evaledSubs 1
                min = minimum . map (numSigFigs . snd) $ measured
             in Right . forceSF min $ fromRational s
evaluate (Exp b e) = do
  res <- evaluate b
  case res of
    (Measured sf bd) -> Right $ forceSF sf (bd ^ e)
    (Constant a) -> Right . Constant $ a ^ e
evaluate (Apply Log10 e) = do
  res <- evaluate e
  case res of
    (Measured sf bd) ->
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

computeUnconstrained :: [(Op, Term)] -> Rational -> Rational
computeUnconstrained terms identity =
  foldl' (uncurry . flip doOp) identity (second extractRat <$> terms)
  where
    extractRat (Measured _ v) = toRational v
    extractRat (Constant v) = v

doOp :: Op -> Rational -> Rational -> Rational
doOp Add a b = a + b
doOp Sub a b = a - b
doOp Mul a b = a * b
doOp Div a b = a / b

forceSF :: Integer -> BigDecimal -> Term
forceSF sf' bd = Measured sf' . roundToPlace bd . rightmostSignificantPlace sf' $ bd

evaluateSubtrees :: [(a, Expr)] -> Either Text [(a, Term)]
evaluateSubtrees xs = traverse sequenceA $ second evaluate <$> xs
