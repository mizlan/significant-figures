{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A module to unparse an expression.
module Data.SigFig.PrettyPrint (prettyPrint) where

import Data.BigDecimal (BigDecimal (..))
import Data.BigDecimal qualified as BD
import Data.SigFig.Types hiding (div)
import Data.SigFig.Util (display, isTerminating)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Real (Ratio(..))

precedence = \case
  Apply {} -> 10
  Literal {} -> 11
  Exp {} -> 3
  Prec2 {} -> 2
  Prec1 {} -> 1

-- the only time we don't need parentheses is with leaf or if child is higher precedence

precede :: Int -> Op -> Expr -> Text
precede prec Add = (" + " <>) . prettyPrintPrec prec
precede prec Sub = (" - " <>) . prettyPrintPrec prec
precede prec Mul = (" * " <>) . prettyPrintPrec prec
precede prec Div = (" / " <>) . prettyPrintPrec prec

printTerm :: Term -> Text
printTerm (Measured sf bd) = format bd
  where
    ssf = T.pack $ show sf
    format :: BigDecimal -> Text
    format term' =
      let term@(BigDecimal v s') = BD.nf term'
          s = fromIntegral s' :: Integer
          termText = T.pack . show $ term
          p = fromIntegral (BD.precision term) :: Integer
          rsdp = p - sf - s
          rsd = if sf > p then 0 else v `div` (10 ^ (rsdp + s)) `mod` 10
       in if rsd /= 0 || rsdp == 0 && p == 1
            then termText
            else
              if rsdp >= 1
                then let coef = BigDecimal v (fromIntegral (s + (p - 1))) in format coef <> "e" <> T.pack (show $ p - 1)
                else
                  termText
                    <> (if s > 0 then "" else ".")
                    <> T.replicate (fromIntegral $ sf - p) "0"
printTerm (Constant v@(a :% b)) =
  T.pack $
    if isTerminating b
      then (++ "c") . show . BD.nf $ fromRational v
      else "(" ++ show a ++ "c / " ++ show b ++ "c)"

conditionallyAddParens :: Int -> Int -> Text -> Text
conditionallyAddParens outer inner t = if inner > outer then t else "(" <> t <> ")"

printFunc Log10 x = "log(" <> prettyPrintPrec 0 x <> ")"
printFunc Antilog10 x = "exp(" <> prettyPrintPrec 0 x <> ")"

prettyPrintPrec :: Int -> Expr -> Text
prettyPrintPrec prec e =
  let prec' = precedence e
   in case e of
        Literal n -> printTerm n
        Prec1 ((_, x) : xs) -> conditionallyAddParens prec prec' $ prettyPrintPrec 1 x <> foldMap (uncurry $ precede 1) xs
        Prec2 ((_, x) : xs) -> conditionallyAddParens prec prec' $ prettyPrintPrec 2 x <> foldMap (uncurry $ precede 2) xs
        Exp a b -> conditionallyAddParens prec prec' $ prettyPrintPrec 3 a <> " ** " <> prettyPrintPrec 3 b
        Apply a b -> printFunc a b
        _ -> error "ill-formed expression"

-- | Pretty print an expression, adding parentheses where needed. Text emitted from the
-- pretty printer is intended to be able to be re-parsed, into the same expression tree.
--
-- ==== __Examples__
--
-- If you want to create expressions to pretty print, utilize the
-- functions in 'Data.SigFig.Types' like below to make life easier.
--
-- >>> prettyPrint $ lMeasured 3 4.0
-- "4.00"
--
-- >>> prettyPrint $ add [lConstant 3, lMeasured 2 3.5]
-- "3c + 3.5"
--
-- >>> prettyPrint $ add [lConstant 3, mul [lMeasured 2 3.5, lConstant 2.7]]
-- "3c + 3.5 * 2.7c"
--
-- >>> prettyPrint $ mul [lConstant 3, add [lMeasured 2 3.5, lConstant 2.7]]
-- "3c * (3.5 + 2.7c)"
prettyPrint :: Expr -> Text
prettyPrint = prettyPrintPrec 0
