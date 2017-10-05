{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable,
  TypeSynonymInstances, MultiParamTypeClasses #-}

----------------------------------------------------------------------------
-- |
-- Module       : Conf.Bindings.Keys.Launchers
-- Copyright    : (c) maddy@na.ai
-- License      : MIT
--
-- Maintainer   : maddy@na.ai
-- Stability    : unstable
-- Portability  : unportable
--
----------------------------------------------------------------------------
module Conf.Bindings.Keys.Launchers
  ( launchers
  ) where

import qualified Conf.Applications as Apps
import qualified Conf.NamedScratchpads as NS

import Conf.Bindings.Keys.Internal (subKeys)

import qualified XMonad.ManageHook as ManageHook

import XMonad (spawn)
import XMonad.Actions.WindowGo (raiseNextMaybe)
import XMonad.Util.NamedActions (addName)

import XMonad.ManageHook ((=?))

launchers c = subKeys "Launchers" c
  [ ("M-<Space>",  addName "Launcher" $ spawn Apps.launcher)
  , ("M-<Return>", addName "Terminal" $ spawn Apps.terminal)
  , ("M-d",        addName "Dirciple" $ spawn Apps.dirciple)
  , ("M-v",        addName "Editor"   $ spawn Apps.editor)
  , ("M-\\",       addName "Browser"  $ spawn Apps.browser)

  , ("M-S-e",        addName "Select or Launch Email" $ rrc Apps.email Apps.emailClass)

  -- NamedScratchpads
  , ("M-p",               addName "Enpass"                  $ NS.action Apps.enpassClass)
  , ("M-S-p",             addName "1Password"               $ NS.action Apps.onepassClass)
  , ("M-/",               addName "Terminal Scratchpad"     $ NS.action Apps.terminalClass)
  , ("M-s",               addName "Slack Scratchpad"        $ NS.action Apps.slackClass)
  , ("M-c",               addName "Weechat Scratchpad"      $ NS.action Apps.weechatClass)
  , ("M-S-c",             addName "Discord Scratchpad"      $ NS.action Apps.discordClass)
  , ("M-S-f",             addName "File Manager Scratchpad" $ NS.action Apps.filemanagerClass)
  , ("M-t",               addName "Task Scratchpad"         $ NS.action Apps.taskClass)
  , ("M-<XF86AudioPlay>", addName "Music Player Scratchpad" $ NS.action Apps.musicClass)
  ]

rrc a c = raiseNextMaybe (spawn a) (className c)
className n = ManageHook.className =? n
