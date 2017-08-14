----------------------------------------------------------------------------
-- |
-- Module       : Conf.Theme.Fonts
-- Copyright    : (c) maddy@na.ai
-- License      : MIT
--
-- Maintainer   : maddy@na.ai
-- Stability    : unstable
-- Portability  : unportable
--
----------------------------------------------------------------------------
module Conf.Theme.Fonts
  ( font
  , titleFont
  -- , tabFont
  , bigFont
  , wideFont
  ) where

getFont :: String -> Int -> String
getFont f s = f ++ ":size=" ++ (show s)

getFontPx :: String -> Int -> String
getFontPx f s = f ++ ":pixelsize=" ++ (show s)

withStyle :: String -> String -> String
withStyle f s = f ++ ":style=" ++ s

dejaVu   = "xft:DejaVu Sans Mono for Powerline:hinting=true"
euroStar = "xft:Eurostar Black Extended:hinting=true"

font      = getFont dejaVu 8 --dejaVu   ++ ":size=10"
titleFont = getFont dejaVu 8 -- dejaVu   ++ ":size=8"
-- tabFont   = dejaVu   ++ ":style=Bold:size=8"
bigFont   = getFont dejaVu 14 --dejaVu   ++ ":size=14"
-- wideFont  = euroStar ++ ":style=Regular:pixelsize=180"
wideFont  = getFontPx (withStyle euroStar "Regular") 180
