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
