----------------------------------------------------------------------------
-- |
-- Module       : Conf.Theme.Colors
-- Copyright    : (c) maddy@na.ai
-- License      : MIT
--
-- Maintainer   : maddy@na.ai
-- Stability    : unstable
-- Portability  : unportable
--
----------------------------------------------------------------------------
module Conf.Theme.Colors where

---- Color Classes

normalBorder  = base03
focusedBorder = active
active        = skyblue
activeWarn    = red
inactive      = base02
focus         = skyblue
unfocus       = base03

---- Named Colors

base03  = color11 -- Background Darkest
base02  = color0  -- Background Dark

base01  = color15 -- Content Darkest
base00  = color7  -- Content Dark

base0   = color13 -- Content Light
base1   = white   -- Content Lightest

base2   = color10 -- Background Light
base3   = color8  -- Background Lightest

teal    = color14
cyan    = color6
skyblue = color4
blue    = color12

yellow  = color3
orange  = color9
red     = color1
green   = color2
violet  = color5

---- Raw colors

-- Nord

black   = "#000000"
white   = "#FFFFFF"

color0  = "#3B4252"
color1  = "#BF616A"
color2  = "#A3BE8C"
color3  = "#EBCB8B"
color4  = "#81A1C1"
color5  = "#B48EAD"
color6  = "#88C0D0"
color7  = "#E5E9F0"
color8  = "#4C566A"
color9  = "#D08770"
color10 = "#434C5E"
color11 = "#2E3440"
color12 = "#5E81AC"
color13 = "#ECEFF4"
color14 = "#8FBCBB"
color15 = "#D8DEE9"
