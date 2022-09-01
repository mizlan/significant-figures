{-# LANGUAGE BlockArguments #-}

-- |
-- Module      :  Data.SigFig
-- License     :  GPL-3.0-or-later (see the LICENSE file)
--
-- Maintainer  :  michaellan202@gmail.com
--
-- This module exports everything you need to work with significant figures, including parsing and evaluation.
module Data.SigFig
  ( -- * Types
    Term (..),
    Op (..),
    Expr (..),
    Function (..),

    -- * High-Level Functions
    parseEval,
    parse,
    parse',
    evaluate,
    evaluate',
    display,
    displayInformational,
    displayFull,
    processExpression,

    -- * Creating and Manipulating Terms and Expressions
    measured,
    constant,
    l,
    lMeasured,
    lConstant,
    add,
    sub,
    mul,
    div,
    exp,
    apply,
  )
where

import Data.SigFig.Evaluate
import Data.SigFig.Interface
import Data.SigFig.Parse
import Data.SigFig.Types
import Data.SigFig.Util
import Prelude hiding (div, exp)
