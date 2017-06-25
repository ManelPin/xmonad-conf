{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable,
  TypeSynonymInstances, MultiParamTypeClasses #-}

----------------------------------------------------------------------------
-- |
-- Module       : Conf.Bindings.Keys.Internal
-- Copyright    : (c) maddy@na.ai
-- License      : MIT
--
-- Maintainer   : maddy@na.ai
-- Stability    : unstable
-- Portability  : unportable
--
----------------------------------------------------------------------------
module Conf.Bindings.Keys.Internal where

import qualified XMonad
import qualified XMonad.Actions.MessageFeedback as MessageFeedback
import qualified XMonad.Util.EZConfig as EZConfig
import qualified XMonad.Util.NamedActions as NamedActions

import XMonad.Layout.WindowNavigation (Direction2D(D, U, L, R))

subKeys str conf ks = NamedActions.subtitle str : EZConfig.mkNamedKeymap conf ks

zipM  m nm ks as f   = zipWith (\k d -> (m ++ k, NamedActions.addName nm $ f d)) ks as
zipM' m nm ks as f b = zipWith (\k d -> (m ++ k, NamedActions.addName nm $ f d b)) ks as

dirKeys   = [ "j" ,  "k" ,  "h" ,  "l" ]
arrowKeys = ["<D>", "<U>", "<L>", "<R>"]
dirs      = [  D  ,   U  ,   L  ,   R  ]

wsKeys = map show $ [1 .. 9] ++ [0]

tryMsgR x y = sequence_ [(MessageFeedback.tryMessage_ x y), XMonad.refresh]
