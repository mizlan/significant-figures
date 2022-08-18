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
import Web.Spock
import Web.Spock.Config
import Prelude hiding (id)

frontPage :: H.Html
frontPage = H.docTypeHtml do
  H.head do
    H.title "Sig Figs!"
    H.script ! src "/public/index.js" ! defer mempty $ mempty
    H.link ! rel "stylesheet" ! type_ "text/css" ! href "/public/styles.css"
    H.link ! rel "preconnect" ! href "https://fonts.googleapis.com"
    H.link ! rel "preconnect" ! href "https://fonts.gstatic.com"
    H.link ! href "https://fonts.googleapis.com/css2?family=DM+Serif+Display&display=swap" ! rel "stylesheet"
  H.body do
    H.div ! id "content" $ do
      H.h1 "Significant Figures Calculator"
      H.p ! id "box" $ ""
      H.form ! id "calc" ! action "calc" $ do
        H.input ! id "expr" ! type_ "text" ! name "expr"
