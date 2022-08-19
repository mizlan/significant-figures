{-# LANGUAGE OverloadedStrings #-}

module Site.CSS where

import Clay
import Prelude hiding (id, rem)

styles :: Css
styles = do
  html
    ? fontSize (px 21)
  star
    ? color "#57514b"
  star # selection
    ? backgroundColor "#d1c1b0"
  body
    ? backgroundColor "#ebe2d8"
  "#content" ? do
    width (rem 40)
    marginLeft auto
    marginRight auto
    paddingTop (vw 10)
  -- display flex
  -- justifyContent center
  -- alignItems center
  -- flexDirection column
  -- position relative
  h1 ? do
    color "#57514b"
    fontFamily ["DM Serif Display"] [serif]
  input ? do
    boxSizing borderBox
    width (rem 20)
    fontFamily [] [monospace]
    fontSize (rem 1)
    marginBottom (px 0)
    backgroundColor "#e3d7c9"
    borderWidth 0
    color "#57514b"
    borderRadius (rem 0.3) (rem 0.3) (rem 0) (rem 0)
    padding (rem 1.0) (rem 0.6) (rem 0.65) (rem 0.6)

    focus & do
      "outline" -: "none"

  "#cover" ? do
    width (rem 20)
    backgroundColor "#e3d7c9"
    height (rem 0.25)
    borderRadius (rem 0.0) (rem 0.0) (rem 0.3) (rem 0.3)
    marginTop (px 0)

  "#wrapper"
    & position relative

  "#stick" & do
    width (pct 0)
    height (rem 0.25)
    bottom (rem 0)
    display block
    borderRadius (rem 0.0) (rem 0.0) (rem 0.3) (rem 0.3)
    backgroundColor "#ab9985"
    position absolute
    left (rem 10)
    transitionDurations [sec 0.5, sec 0.5]
    transitionProperties ["width", "left"]

  input # focus |~ "#stick" ? do
    width (rem 20)
    left (rem 0)

  details ? do
    fontFamily [] [monospace]
    marginTop (rem 2)

    summary
      ? fontFamily [] [monospace]
