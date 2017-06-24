{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses #-}
----------------------------------------------------------------------------
-- |
-- Module       : Conf.Applications
-- Copyright    : (c) maddy@na.ai
-- License      : MIT
--
-- Maintainer   : maddy@na.ai
-- Stability    : unstable
-- Portability  : unportable
--
----------------------------------------------------------------------------
module Conf.Applications
  ( terminal
  , browser
  , browserClass
  , statusBar
  , launcher
  ) where

terminal     = "urxvt"
browser      = "chromium"
browserClass = "Google-chrome-beta"
statusBar    = "xmobar $HOME/.xmonad/xmobar.conf"
launcher     = "rofi -matching fuzzy -modi combi -show combi -combi-modi run,drun"
