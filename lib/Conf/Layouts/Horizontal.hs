{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses, NoMonomorphismRestriction, FlexibleContexts #-}

----------------------------------------------------------------------------
-- |
-- Module       : Conf.Layouts.Horizontal
-- Copyright    : (c) maddy@na.ai
-- License      : MIT
--
-- Maintainer   : maddy@na.ai
-- Stability    : unstable
-- Portability  : unportable
--
----------------------------------------------------------------------------
module Conf.Layouts.Horizontal
  ( horizontal
  , name
  ) where

import Conf.Layouts.Internal (addTopBar, named)

import Conf.Theme.Gaps (gaps, spacing)

import qualified XMonad.Layout.Row as Row

import XMonad.Hooks.ManageDocks (avoidStruts)

name = "HZ"

horizontal
  = named name
  $ avoidStruts
  $ addTopBar
  $ gaps
  $ spacing
  $ Row.Row 1

