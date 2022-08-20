{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Clay (render)
import Control.Exception (IOException, catch)
import Data.Aeson hiding (json)
import Data.SigFig
import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as L
import Data.Text.Lazy.Encoding qualified as L
import GHC.Generics
import GHC.TypeLits (ErrorMessage (Text))
import Language.Javascript.JMacro (renderJs)
import Site.HTML
import Site.CSS (styles)
import Site.JS (frontpageJS)
import System.Environment (getEnv)
import Text.Blaze.Html.Renderer.Text qualified as R
import Text.Blaze.Html5 qualified as H
import Text.Read (readMaybe)
import Web.Internal.HttpApiData
import Web.Spock
import Web.Spock.Config

type Api = SpockM () () () ()

type ApiAction a = SpockAction () () () a

-- | Datatype representing API calculation. If sigfigs is not a number or,
-- is not present, that means the number is a constant. Otherwise it's a measured number.
data Calculation = Calculation
  { ok :: Bool,
    output :: Text,
    annotation :: Maybe Text
  }
  deriving (Generic, Show)

newtype CalculationRequest = CalculationRequest
  { input :: Text
  }
  deriving (Generic, Show)

instance ToJSON Calculation

instance FromJSON CalculationRequest

instance FromHttpApiData CalculationRequest where
  parseUrlPiece = pure . CalculationRequest

app :: Api
app = do
  get root do
    html . L.toStrict $ R.renderHtml frontPage
  get "public/index.js" do
    setHeader "Content-Type" "application/javascript"
    lazyBytes . L.encodeUtf8 . L.pack . show . renderJs $ frontpageJS
  get "public/styles.css" do
    setHeader "Content-Type" "text/css"
    lazyBytes . L.encodeUtf8 . render $ styles
  get "calc" do
    (CalculationRequest e) <- param' "expr" :: ApiAction CalculationRequest
    json $ case parseEval e of
      Right t ->
        let (a, b) = displayInformational t
         in Calculation True a (Just b)
      Left e -> Calculation False e Nothing
  post "calc" do
    (CalculationRequest e) <- jsonBody' :: ApiAction CalculationRequest
    json $ case parseEval e of
      Right t ->
        let (a, b) = displayInformational t
         in Calculation True a (Just b)
      Left e -> Calculation False e Nothing

main :: IO ()
main = do
  cfg <- defaultSpockCfg () PCNoDatabase ()
  portStr <- getEnv "PORT" `catch` (const $ pure "8080" :: IOException -> IO String)
  case readMaybe portStr of
    Nothing -> print "PORT is not a valid number"
    Just port -> runSpock port $ spock cfg app
