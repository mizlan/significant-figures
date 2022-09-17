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
precede prec Add = (" + " <>) . prettyPrint prec
precede prec Sub = (" - " <>) . prettyPrint prec
precede prec Mul = (" * " <>) . prettyPrint prec
precede prec Div = (" / " <>) . prettyPrint prec

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

printFunc Log10 x = "log(" <> prettyPrint 0 x <> ")"
printFunc Antilog10 x = "exp(" <> prettyPrint 0 x <> ")"

prettyPrint :: Int -> Expr -> Text
prettyPrint prec e =
  let prec' = precedence e
   in case e of
        Literal n -> printTerm n
        Prec1 ((_, x) : xs) -> conditionallyAddParens prec prec' $ prettyPrint 1 x <> foldMap (uncurry $ precede 1) xs
        Prec2 ((_, x) : xs) -> conditionallyAddParens prec prec' $ prettyPrint 2 x <> foldMap (uncurry $ precede 2) xs
        Exp a b -> conditionallyAddParens prec prec' $ prettyPrint 3 a <> " ** " <> prettyPrint 3 b
        Apply a b -> printFunc a b
        _ -> error "ill-formed expression"
