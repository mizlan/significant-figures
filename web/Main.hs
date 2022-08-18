{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (IOException, catch)
import Data.Aeson hiding (json)
import Data.SigFig
import Data.Text (Text, pack)
import Data.Text.Lazy (toStrict)
import GHC.Generics
import GHC.TypeLits (ErrorMessage (Text))
import Site
import System.Environment (getEnv)
import Text.Blaze.Html.Renderer.Text qualified as R
import Text.Blaze.Html5 qualified as H
import Text.Read (readMaybe)
import Web.Spock
import Web.Spock.Config

type Api = SpockM () () () ()

type ApiAction a = SpockAction () () () a

-- | Datatype representing API calculation. If sigfigs is not a number or,
-- is not present, that means the number is a constant. Otherwise it's a measured number.
data Calculation = Calculation
  { ok :: Bool,
    output :: Text,
    sigfigs :: Maybe Int
  }
  deriving (Generic, Show)

newtype CalculationRequest = CalculationRequest
  { input :: Text
  }
  deriving (Generic, Show)

instance ToJSON Calculation

instance FromJSON CalculationRequest

app :: Api
app = do
  get root do
    html . toStrict $ R.renderHtml frontPage
  getpost "calc" do
    (CalculationRequest e) <- jsonBody' :: ApiAction CalculationRequest
    json $ case parseEval e of
      Right t -> Calculation True (display t) $ case t of
        Measured sf bd -> Just $ fromIntegral sf
        Constant ra -> Nothing
      Left e -> Calculation False e Nothing

main :: IO ()
main = do
  cfg <- defaultSpockCfg () PCNoDatabase ()
  portStr <- getEnv "PORT" `catch` (const $ pure "8080" :: IOException -> IO String)
  case readMaybe portStr of
    Nothing -> print "PORT is not a valid number"
    Just port -> runSpock port $ spock cfg app
