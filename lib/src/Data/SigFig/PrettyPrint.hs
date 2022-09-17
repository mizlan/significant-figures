{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A module to unparse an expression.
module Data.SigFig.PrettyPrint (prettyPrint) where

import Data.SigFig.Types
import Data.Text (Text)
import Data.SigFig.Util (display)

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
printTerm = display

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
