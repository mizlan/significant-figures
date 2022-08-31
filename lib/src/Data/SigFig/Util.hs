{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune #-}

-- |
-- A set of (very useful) utility functions, which notably include
-- 'display' and 'displayInformational'.
module Data.SigFig.Util where

import Data.BigDecimal (BigDecimal (..))
import Data.BigDecimal qualified as BD
import Data.SigFig.Types hiding (div)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Natural (naturalFromInteger)
import GHC.Real (Ratio ((:%)), (%))

-- | Get the rightmost significant decimal place given a number of
-- significant figures @sf@ and a BigDecimal @bd@. It is as if one
-- were calculating the value for a @Measured sf bd@.
--
-- A negative return value is allowed and meaningful.
--
-- >>> rightmostSignificantPlace 2 (BigDecimal 42 1)
-- -1
rightmostSignificantPlace :: Integer -> BigDecimal -> Integer
rightmostSignificantPlace sf bd =
  let v' = BD.nf bd
      dp = BD.scale v'
      p = BD.precision v'
   in fromIntegral p - sf - fromIntegral dp

-- | Force a given BigDecimal to have a certain number of significant
-- decimal places. A positive integer means to the left of decimal place,
-- negative means to the right.
--
-- >>> forceDP (-2) (fromRational 123.456)
-- Measured {numSigFigs = 5, value = 123.46}
-- >>> forceDP 2 (fromRational 123.456)
-- Measured {numSigFigs = 1, value = 100}
forceDP :: Integer -> BigDecimal -> Term
forceDP dp bd =
  let res = BD.nf $ roundToPlace bd dp
   in Measured (max 0 $ fromIntegral (BD.precision res) - fromIntegral (BD.scale res) - dp) res

-- | Force a given BigDecimal to have a certain number of significant figures.
-- A positive integer means to the left of decimal place, negative means to the right.
forceSF :: Integer -> BigDecimal -> Term
forceSF sf' bd = Measured sf' . roundToPlace bd . rightmostSignificantPlace sf' $ bd

-- | Round a BigDecimal to a specified decimal place. A positive integer means
-- to the left of decimal place, negative means to the right.
--
-- >>> roundToPlace (BigDecimal 421 1) (0)
-- 42
roundToPlace :: BigDecimal -> Integer -> BigDecimal
roundToPlace bd@(BigDecimal v s) dp
  | dp < 0 = BD.roundBD bd $ BD.halfUp (fromIntegral (- dp))
  | otherwise =
    let bd' = BigDecimal v (s + fromIntegral dp)
     in BD.roundBD bd' (BD.halfUp 0) * 10 ^ dp

-- | Given a term, display it in the most convenient way possible. This means,
-- if the normal representation of the number accurately represents how many
-- significant figures it has, then display it normally. Adds trailing zeroes
-- if necessary to floats and opts for scientific notation if necessary.
--
-- >>> display $ measured 3 200
-- "200."
-- >>> display $ measured 3 4
-- "4.00"
-- >>> display $ measured 2 400
-- "4.0 x 10^2"
-- >>> display $ measured 2 430
-- "430"
-- >>> display $ measured 1 1
-- "1"
-- >>> display $ constant (3 % 8)
-- "0.375"
-- >>> display $ constant (4 % 9)
-- "4/9"
-- >>> display $ measured 2 4.3
-- "4.3"
display :: Term -> Text
display (Measured sf bd) = format bd
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
                then let coef = BigDecimal v (fromIntegral (s + (p - 1))) in format coef <> " x 10^" <> T.pack (show $ p - 1)
                else
                  termText
                    <> (if s > 0 then "" else ".")
                    <> T.replicate (fromIntegral $ sf - p) "0"
display (Constant v@(a :% b)) =
  T.pack $
    if isTerminating b
      then show . BD.nf $ fromRational v
      else show a ++ "/" ++ show b

-- | Used in the CLI. Not super pretty but gets the job done in terms of displaying enough information.
--
-- >>> displayFull (constant 3.45)
-- "3.45 (const)"
-- >>> displayFull (measured 3 8500)
-- "8.50 x 10^3 (3 s.f.)"
displayFull :: Term -> Text
displayFull t@(Measured sf bd) = display t <> annot
  where
    annot = " (" <> T.pack (show sf) <> " s.f.)"
displayFull t@(Constant (a :% b)) = display t <> annot
  where
    annot = if isTerminating b then " (const)" else " (non-terminating const)"

-- | Given a term, return a tuple where the first element is the output of display and the second is an annotation of the type of value. Used in the API.
--
-- >>> displayInformational $ constant 3
-- ("3","constant value")
-- >>> displayInformational $ measured 2 3.4
-- ("3.4","2 significant figures")
-- >>> displayInformational $ measured 3 3400
-- ("3.40 x 10^3","3 significant figures")
displayInformational :: Term -> (Text, Text)
displayInformational t@(Measured sf bd) = (display t, annot)
  where
    annot = T.pack (show sf) <> " significant figure" <> if sf /= 1 then "s" else mempty
displayInformational t@(Constant (a :% b)) = (display t, annot)
  where
    annot = if isTerminating b then "constant value" else "constant, non-terminating decimal value"

-- | Given a denominator, tell if the decimal expansion of the fraction terminates.
-- Useful for telling whether a constant value is a terminating or non-terminating value.
-- But one should probably use 'displayInformational' to extract such information.
isTerminating :: Integer -> Bool
isTerminating = (== 1) . stripFactor 5 . stripFactor 2
  where
    stripFactor d n = case n `quotRem` d of
      (q, 0) -> stripFactor d q
      _ -> n
