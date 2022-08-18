{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Site where

import Data.Text.Lazy (toStrict)
import Text.Blaze.Html.Renderer.Text qualified as R
import Text.Blaze.Html5 qualified as H
import Web.Spock
import Web.Spock.Config

frontPage :: H.Html
frontPage = H.docTypeHtml do
  H.head do
    H.title "Sig Figs!"
  H.body do
    H.p "hi! just testing"
