{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune #-}

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
-- >>> display (Measured 3 (BigDecimal 200 0))
-- "200."
-- >>> display (Measured 3 (BigDecimal 4 0))
-- "4.00"
-- >>> display (Measured 2 (BigDecimal 400 0))
-- "4.0 x 10^2"
-- >>> display (Measured 2 (BigDecimal 430 0))
-- "430"
-- >>> display (Measured 1 (BigDecimal 1 0))
-- "1"
-- >>> display (Constant (3 % 8))
-- "0.375"
-- >>> display (Constant (4 % 9))
-- "4/9"
-- >>> display (Measured 2 (BigDecimal 43 1))
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

maxNonInfiniteDouble :: Double
maxNonInfiniteDouble = encodeFloat m n
  where
    b = floatRadix (0 :: Double)
    e = floatDigits (0 :: Double)
    (_, e') = floatRange (0 :: Double)
    m = b ^ e - 1
    n = e' - e

-- | Used in the CLI
displayFull :: Term -> Text
displayFull t@(Measured sf bd) = display t <> annot
  where
    annot = " (" <> T.pack (show sf) <> " s.f.)"
displayFull t@(Constant (a :% b)) = display t <> annot
  where
    annot = if isTerminating b then " (const)" else " (non-terminating const)"

-- Given a denominator, tell if the decimal expansion of the fraction terminates
isTerminating :: Integer -> Bool
isTerminating = (== 1) . stripFactor 5 . stripFactor 2
  where
    stripFactor d n = case n `quotRem` d of
      (q, 0) -> stripFactor d q
      _ -> n

-- | Given a term, return a tuple where the first element is the output of display and the second is an annotation of the type of value. Used in the API.
--
-- >>> displayInformational (Constant 3)
-- ("3","constant value")
-- >>> let b = BD.fromString "3.4" in displayInformational (Measured 2 b)
-- ("3.4","2 significant figures")
-- >>> let b = BD.fromString "3400" in displayInformational (Measured 3 b)
-- ("3.40 x 10^3","3 significant figures")
displayInformational :: Term -> (Text, Text)
displayInformational t@(Measured sf bd) = (display t, annot)
  where
    annot = T.pack (show sf) <> " significant figure" <> if sf /= 1 then "s" else mempty
displayInformational t@(Constant (a :% b)) = (display t, annot)
  where
    annot = if isTerminating b then "constant value" else "constant, non-terminating decimal value"
