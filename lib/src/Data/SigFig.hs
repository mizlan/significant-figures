{-# LANGUAGE BlockArguments #-}

-- |
-- Module      :  Data.SigFig
-- License     :  GPL-3.0-or-later (see the LICENSE file)
--
-- Maintainer  :  michaellan202@gmail.com
--
-- This module exports everything you need to work with significant figures, including parsing and evaluation.
module Data.SigFig
  ( Term (..),
    Op (..),
    Expr (..),
    Function (..),
    processExpression,
    parseEval,
    display,
    displayFull,
    displayInformational,
    evaluate,
    parse,
    l,
    lMeasured,
    lConstant,
    add,
    sub,
    mul,
    div,
    exp,
    apply,
    measured,
    constant,
  )
where

import Data.SigFig.Evaluate
import Data.SigFig.Interface
import Data.SigFig.Parse
import Data.SigFig.Types
import Data.SigFig.Util
import Prelude hiding (div, exp)
