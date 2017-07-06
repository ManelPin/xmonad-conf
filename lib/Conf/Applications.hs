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
module Conf.Applications where

terminal     = "termite"
browser      = "gtk-launch chromium"
browserClass = "Chromium"
passwordMgr  = "gtk-launch enpass"
email        = "gtk-launch thunderbird"

statusBar    = "xmobar $HOME/.xmonad/xmobar.conf"
launcher     = "rofi -matching fuzzy -modi combi -show combi -combi-modi run,drun"
