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

dejaVu   = "xft:DejaVu Sans Mono for Powerline:hinting=true"
euroStar = "xft:Eurostar Black Extended:hinting=true"

font      = dejaVu   ++ ":size=10"
titleFont = dejaVu   ++ ":size=8"
-- tabFont   = dejaVu   ++ ":style=Bold:size=8"
bigFont   = dejaVu   ++ ":size=14"
wideFont  = euroStar ++ ":style=Regular:pixelsize=180"
