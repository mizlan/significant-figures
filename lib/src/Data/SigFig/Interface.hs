{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.SigFig.Interface where

import Data.SigFig.Evaluate
import Data.SigFig.Parse
import Data.SigFig.Types
import Data.SigFig.Util
import Data.Text (Text)
import Data.Text qualified as T
import Text.Parsec
import Data.Bifunctor (first)

-- | Takes an expression in text and returns either an error message or an evaluated term
parseEval :: Text -> Either Text Term
parseEval e = textify (parse fullExpr "" e) >>= evaluate
  where textify = first (T.pack . show)

-- | A convenience function for use in REPLs. Returns text that can either signify a
-- result or error.
processExpression :: Text -> Text
processExpression e = either ("Error: " <>) displayFull $ parseEval e
