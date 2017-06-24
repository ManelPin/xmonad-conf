{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable,
  TypeSynonymInstances, MultiParamTypeClasses #-}

----------------------------------------------------------------------------
-- |
-- Module       : Conf.Layouts.Nav2D
-- Copyright    : (c) maddy@na.ai
-- License      : MIT
--
-- Maintainer   : maddy@na.ai
-- Stability    : unstable
-- Portability  : unportable
--
----------------------------------------------------------------------------
module Conf.Layouts.Nav2D
  ( nav2D
  ) where

import qualified XMonad.Actions.Navigation2D as Navigation2D
import qualified XMonad.Prompt as Prompt

nav2D =
  Prompt.def
  { Navigation2D.defaultTiledNavigation = Navigation2D.centerNavigation
  , Navigation2D.floatNavigation = Navigation2D.centerNavigation
  , Navigation2D.screenNavigation = Navigation2D.lineNavigation
  , Navigation2D.layoutNavigation =
      [ ("Full", Navigation2D.centerNavigation)
      ]
  , Navigation2D.unmappedWindowRect =
      [ ("Full", Navigation2D.singleWindowRect)
      ]
  }
