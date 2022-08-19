{-# LANGUAGE OverloadedStrings #-}

module Site.CSS where

import Clay
import Prelude hiding (id, rem)

styles :: Css
styles = do
  html ? fontSize (px 21)
  star ? do
    color "#57514b"
    fontFamily ["Inter"] [serif]
  star # selection ? backgroundColor "#d1c1b0"
  body ? backgroundColor "#ebe2d8"
  code ? do
    backgroundColor "#e3d7c9"
    fontSize (rem 0.83)
    sym margin (rem 0.2)
    sym2 padding (rem 0.1) (rem 0.3)
    sym borderRadius (rem 0.25)
    color "#6b6158"
    fontFamily ["JetBrains Mono"] [monospace]
  "#content" ? do
    width (rem 40)
    marginLeft auto
    marginRight auto
    paddingTop (vw 10)
  h1 ? do
    color "#57514b"
    fontFamily ["DM Serif Display"] [serif]
  input ? do
    boxSizing borderBox
    width (rem 20)
    fontFamily ["JetBrains Mono"] [monospace]
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
    marginTop (rem 2)

    summary ? do
      sym padding (rem 0.5)
      paddingLeft (px 0)

  footer ? do
    position fixed
    bottom (px 0)
    paddingBottom (vw 3)
    color "#6b6158"

  a ? do
    hover & do
      backgroundColor "#e3d7c9"
