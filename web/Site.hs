{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Site where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy (toStrict)
import Site.JS
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Text qualified as R
import Text.Blaze.Html5 (toHtml, (!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html5.Attributes qualified as A
import Web.Spock
import Web.Spock.Config
import Prelude hiding (id)

frontPage :: H.Html
frontPage = H.docTypeHtml do
  H.head do
    H.title "Sig Figs!"
    H.script ! src "/public/index.js" ! defer mempty $ mempty
  H.body do
    H.h1 "Significant Figures Calculator"
    H.p ! id "box" $ ""
    H.form ! id "calc" ! action "calc" $ do
      H.input ! id "expr" ! type_ "text" ! name "expr"
