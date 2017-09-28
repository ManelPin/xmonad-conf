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

getFont :: String -> Float -> String
getFont f s = f ++ ":size=" ++ (show s)

getFontPx :: String -> Int -> String
getFontPx f s = f ++ ":pixelsize=" ++ (show s)

withStyle :: String -> String -> String
withStyle f s = f ++ ":style=" ++ s

inputMono = "xft:InputMono Nerd Font:hinting=true"
euroStar  = "xft:Eurostar Black Extended:hinting=true"

font      = getFont inputMono 9
titleFont = getFont inputMono 6
bigFont   = getFont inputMono 14
wideFont  = getFontPx (withStyle euroStar "Regular") 180
