{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

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
-- >>> rightmostSignificantPlace 1 (BigDecimal 20 0)
-- -1
rightmostSignificantPlace sf bd =
  let v' = BD.nf bd
      dp = BD.getScale v'
      nd = BD.precision v'
   in sf + dp - nd

forceDP :: Integer -> BigDecimal -> Term
forceDP dp bd =
  let res = BD.nf $ roundToPlace bd dp
   in Measured (BD.precision res - BD.getScale res - dp) res

-- | Round a BigDecimal to a specified decimal place. A positive integer means
-- to the right of decimal place, negative means to the left
--
-- >>> roundToPlace (BigDecimal 421 2) 1
-- BigDecimal 42 1
roundToPlace :: BigDecimal -> Integer -> BigDecimal
roundToPlace bd@(BigDecimal v s) dp
  | dp < 0 = BD.roundBD bd $ BD.halfUp (-dp)
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
-- "4.3 x 10^2 (2 s.f.)"
-- "1 (1 s.f.)"
-- "0.375 (const)"
-- "4/9 (non-terminating const)"
-- "4.3 (2 s.f.)"
display :: Term -> Text
display (Measured sf bd) = format (BD.nf bd) <> " (" <> ssf <> " s.f.)"
  where
    ssf = T.pack $ show sf
    format :: BigDecimal -> Text
    format term@(BigDecimal v s) =
      let sdp = negate $ rightmostSignificantPlace sf term
       in if sdp > 0
            then
              T.pack (BD.toString term)
                <> if sdp > s
                  then
                    ( if s == 0
                        then "."
                        else ""
                    )
                      <> T.replicate (fromIntegral (sdp - s)) "0"
                  else ""
            else
              let p = BD.precision term
               in if v `mod` (10 ^ (p - sf)) == 0 && v `mod` (10 ^ (p - sf + 1)) /= 0
                    then T.pack (BD.toString bd)
                    else
                      if sf == p
                        && v `mod` 10 == 0
                        && v /= 0 -- print "0", not "0."
                        then T.pack $ BD.toString term <> "."
                        else
                          if sf < p
                            then
                              let coef = BD.nf . BigDecimal v $ s + (p - 1)
                               in format coef <> " x 10^" <> T.pack (show (p - 1))
                            else T.pack $ BD.toString bd
display (Constant v@(a :% b)) =
  T.pack $
    if isTerminating b
      then (BD.toString . BD.nf . fromRational $ v) ++ " (const)"
      else show a ++ "/" ++ show b ++ " (non-terminating const)"
  where
    isTerminating = (== 1) . stripFactor 5 . stripFactor 2
      where
        stripFactor d n = case n `quotRem` d of
          (q, 0) -> stripFactor d q
          _ -> n
