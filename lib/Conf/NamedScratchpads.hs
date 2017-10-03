{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses #-}
----------------------------------------------------------------------------
-- |
-- Module       : Conf.NamedScratchpads
-- Copyright    : (c) maddy@na.ai
-- License      : MIT
--
-- Maintainer   : maddy@na.ai
-- Stability    : unstable
-- Portability  : unportable
--
----------------------------------------------------------------------------
module Conf.NamedScratchpads
  ( namedScratchpads
  , action
  , manage
  ) where

import qualified Conf.Applications as Apps

import qualified XMonad.StackSet as StackSet

import XMonad.Util.NamedScratchpad
        ( NamedScratchpad(NS)
        , customFloating
        , namedScratchpadAction
        , namedScratchpadManageHook
        )

import qualified XMonad.ManageHook as ManageHook

import XMonad.ManageHook ((=?))

namedScratchpads =
  -- Enpass
  [ NS Apps.enpassClass
       Apps.enpass
       (title Apps.enpassClass)
       float2_3x2_3

  -- -- 1Password
  -- , NS Apps.onepassClass
  --      Apps.onepass
  --      (wmName Apps.onepassWMName)
  --      float2_3x2_3

  -- Terminal
  , NS Apps.terminalClass
       Apps.terminalScratch
       (className Apps.terminalScratchClass)
       float2_3x2_3

  -- Slack
  , NS Apps.slackClass
       Apps.slack
       (className Apps.slackClass)
       float3_4x3_4

  -- Discord
  , NS Apps.discordClass
       Apps.discord
       (className Apps.discordClass)
       float3_4x3_4

  -- File Manager
  , NS Apps.filemanagerClass
       Apps.filemanager
       (className Apps.filemanagerClass)
       float3_4x3_4


  -- Weechat
  , NS Apps.weechatClass
       Apps.weechat
       (className Apps.weechatClass)
       float3_4x3_4

  -- Tasks
  , NS Apps.taskClass
       Apps.task
       (className Apps.taskClass)
       float2_3x2_3

  -- Music Player
  , NS Apps.musicClass
       Apps.music
       (className Apps.musicClass)
       float3_4x3_4
  ]

action = namedScratchpadAction     namedScratchpads
manage = namedScratchpadManageHook namedScratchpads

className n = ManageHook.className =? n
title     n = ManageHook.title     =? n
-- appName   n = ManageHook.appName   =? n
-- wmName    n = (ManageHook.stringProperty "WM_NAME") =? n

float2_3x2_3 = customFloating $ StackSet.RationalRect (1/6) (1/6) (2/3) (2/3)
float3_4x3_4 = customFloating $ StackSet.RationalRect (1/12) (1/12) (5/6) (5/6)
