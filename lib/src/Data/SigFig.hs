{-# LANGUAGE BlockArguments #-}

-- |
-- Module      :  Data.SigFig
-- License     :  GPLv3 (see the LICENSE file)
--
-- Maintainer  :  michaellan202@gmail.com
--
-- This module exports everything you need to work with significant figures, including parsing and evaluation.
module Data.SigFig
  ( processExpression,
    parseEval,
    display,
    displayFull,
    displayInformational,
    evaluate,
    parse,
    module Data.SigFig.Types,
  )
where

import Data.SigFig.Evaluate
import Data.SigFig.Interface
import Data.SigFig.Parse
import Data.SigFig.Types
import Data.SigFig.Util
