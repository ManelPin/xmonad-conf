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
  -- Password Manager
  [ NS Apps.passwordMgrClass
       Apps.passwordMgr
       (className Apps.passwordMgrClass)
       float2_3x2_3

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
       float2_3x2_3
  ]

action = namedScratchpadAction     namedScratchpads
manage = namedScratchpadManageHook namedScratchpads

className n = ManageHook.className =? n
-- title     n = ManageHook.title =? n
-- role n      = (ManageHook.stringProperty "WM_WINDOW_ROLE") ?= n

float2_3x2_3 = customFloating $ StackSet.RationalRect (1/6) (1/6) (2/3) (2/3)
float3_4x3_4 = customFloating $ StackSet.RationalRect (1/12) (1/12) (5/6) (5/6)
