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

textify :: Either ParseError a -> Either Text a
textify (Right m) = Right m
textify (Left e) = Left . T.pack . show $ e

parseEval :: Text -> Either Text Term
parseEval e = textify (parse fullExpr "" e) >>= evaluate

-- | A convenience function for use in REPLs. Returns text that can either be a
-- result or error.
processExpression :: Text -> Text
processExpression e = either ("Error: " <>) display $ parseEval e
