{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Data.SigFig.Util where

import Data.BigDecimal (BigDecimal (..))
import Data.BigDecimal qualified as BD
import Data.SigFig.Types
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Real (Ratio ((:%)), (%))

isMeasured (Measured _ _) = True
isMeasured (Constant _) = False

-- negative return value is allowed and meaningful
-- >>> rightmostSignificantPlace 2 (BigDecimal 42 1)
-- -1
rightmostSignificantPlace :: Integer -> BigDecimal -> Integer
rightmostSignificantPlace sf bd =
  let v' = BD.nf bd
      dp = BD.getScale v'
      p = BD.precision v'
   in p - sf - dp

forceDP :: Integer -> BigDecimal -> Term
forceDP dp bd =
  let res = BD.nf $ roundToPlace bd dp
   in Measured (BD.precision res - BD.getScale res - dp) res

-- | Round a BigDecimal to a specified decimal place. A positive integer means
-- to the left of decimal place, negative means to the right
--
-- >>> roundToPlace (BigDecimal 421 1) (0)
roundToPlace :: BigDecimal -> Integer -> BigDecimal
roundToPlace bd@(BigDecimal v s) dp
  | dp < 0 = BD.roundBD bd $ BD.halfUp (- dp)
  | otherwise =
    let bd' = BigDecimal v (s + dp)
     in BD.roundBD bd' (BD.halfUp 0) * 10 ^ dp

-- >>> display (Measured 3 (BigDecimal 200 0))
-- >>> display (Measured 3 (BigDecimal 4 0))
-- >>> display (Measured 2 (BigDecimal 400 0))
-- >>> display (Measured 2 (BigDecimal 430 0))
-- >>> display (Measured 1 (BigDecimal 1 0))
-- >>> display (Constant (3 % 8))
-- >>> display (Constant (4 % 9))
-- >>> display (Measured 2 (BigDecimal 43 1))
-- "200. (3 s.f.)"
-- "4.00 (3 s.f.)"
-- "4.0 x 10^2 (2 s.f.)"
-- "430 (2 s.f.)"
-- "1 (1 s.f.)"
-- "0.375 (const)"
-- "4/9 (non-terminating const)"
-- "4.3 (2 s.f.)"
display :: Term -> Either Text Text
display (Measured sf bd) = if BD.precision bd >= 308 then Left "too large to display" else pure $ format bd
  where
    ssf = T.pack $ show sf
    format :: BigDecimal -> Text
    format term' =
      let term@(BigDecimal v s) = BD.nf term'
          termText = T.pack . BD.toString $ term
          p = BD.precision term
          rsdp = p - sf - s
          rsd = if sf > p then 0 else v `div` (10 ^ (rsdp + s)) `mod` 10
       in if rsd /= 0 || rsdp == 0 && p == 1
            then termText
            else
              if rsdp >= 1
                then let coef = BigDecimal v (s + (p - 1)) in format coef <> " x 10^" <> T.pack (show $ p - 1)
                else
                  termText
                    <> (if s > 0 then "" else ".")
                    <> T.replicate (fromIntegral $ sf - p) "0"
display (Constant v@(a :% b)) =
  if fromRational v > maxNonInfiniteDouble
    then Left "the number you want is greater than the number of all the atoms in the universe, multiplied by the number of all possible entire chess games, multiplied by the number of possible board states in the game of Go (your number is greater than ~1.8 x 10^308)"
    else
      pure $
        T.pack $
          if isTerminating b
            then BD.toString . fromRational $ v
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
displayFull :: Term -> Either Text Text
displayFull t@(Measured sf bd) = (<> annot) <$> display t
  where
    annot = " (" <> T.pack (show sf) <> " s.f.)"
displayFull t@(Constant (a :% b)) = (<> annot) <$> display t
  where
    annot = if isTerminating b then " (const)" else " (non-terminating const)"

-- | Given a denominator, tell if the decimal expansion of the fraction terminates
isTerminating :: Integer -> Bool
isTerminating = (== 1) . stripFactor 5 . stripFactor 2
  where
    stripFactor d n = case n `quotRem` d of
      (q, 0) -> stripFactor d q
      _ -> n

-- | Used in the API. also wtf is this type sygnature rip
displayInformational :: Term -> Either Text (Text, Text)
displayInformational t@(Measured sf bd) = (,annot) <$> display t
  where
    annot = T.pack (show sf) <> " significant figure" <> if sf /= 1 then "s" else mempty
displayInformational t@(Constant (a :% b)) = (,annot) <$> display t
  where
    annot = if isTerminating b then "constant value" else "constant, non-terminating decimal value"

{-

process:
  1. find rightmost significant digit -> x
  2. if x /= 0, just print as-is
  3. if x is in 10s/100s/1000s place etc., convert to sci not
  4. if x in 10ths/100ths/1000ths etc, add decimal place if needed and zeroes

- as-is
  - in integer, rightmost significant figure is nonzero
- add a "." if needed and zero or more trailing zeroes
-

-}
