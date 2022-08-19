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
    H.link ! href "https://fonts.googleapis.com/css2?family=DM+Serif+Display&family=Inter&family=JetBrains+Mono&display=swap" ! rel "stylesheet"
  H.body do
    H.div ! id "content" $
      do
        H.h1 "Significant Figures Calculator"
        H.form ! id "calc" ! action "calc" $ do
          H.div ! id "wrapper" $ do
            H.input ! id "expr" ! type_ "text" ! name "expr" ! placeholder "type an expression..."
            H.div ! id "cover" $ mempty
            H.div ! id "stick" $ mempty
        H.div ! id "box" $ H.pre mempty
        H.details $
          H.summary "Usage"
            <> textToHtml "Type in an expression containing significant figures and operators! For an example, try this:"
            <> H.code "log(10.45) + 3.6200c * (876.45 - 9.4523) / 2c"
            <> toHtml ("and see what you get!" :: Text)
        H.footer $
          textToHtml "made by "
            <> (H.a ! href "https://github.com/mizlan" $ "mizlan")
            <> textToHtml " with ♥"
  where
    textToHtml = toHtml :: Text -> Html
