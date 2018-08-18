{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable,
  TypeSynonymInstances, MultiParamTypeClasses #-}

----------------------------------------------------------------------------
-- |
-- Module       : Conf.Theme.Gaps
-- Copyright    : (c) maddy@na.ai
-- License      : MIT
--
-- Maintainer   : maddy@na.ai
-- Stability    : unstable
-- Portability  : unportable
--
----------------------------------------------------------------------------
module Conf.Theme.Gaps
  ( gaps
  , smallGaps
  , bigGaps
  , spacing
  ) where

import Conf.Theme.Sizes (gap, gapBig, gapSmall)

import XMonad.Layout.Spacing (spacingRaw, Border(..))

import XMonad.Layout.WindowNavigation (Direction2D(D, L, R, U))

import qualified XMonad.Layout.Gaps as Gaps

uniformBorder :: Integer -> Border
uniformBorder i = Border i i i i

spacing = spacingRaw False (uniformBorder g') True (uniformBorder g') True
  where g' = fromIntegral gap

-- TODO: Write a function to generate gaps for given px size
gaps = Gaps.gaps [(U, gap), (D, gap), (L, gap), (R, gap)]

smallGaps = Gaps.gaps [(U, gapSmall), (D, gapSmall), (L, gapSmall), (R, gapSmall)]

bigGaps = Gaps.gaps [(U, gapBig), (D, gapBig), (L, gapBig), (R, gapBig)]
