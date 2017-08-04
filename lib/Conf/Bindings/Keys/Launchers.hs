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
  , ("M-e",        addName "Email"    $ spawn Apps.email)

  -- Only launch if not already running, otherwise cycle through matching applications
  , ("M-S-<Return>", addName "Select or Launch Terminal" $ rrc Apps.terminal Apps.terminalClass)
  , ("M-S-v",        addName "Select or Launch Editor"   $ rrc Apps.editor   Apps.editorClass)
  , ("M-S-\\",       addName "Select or Launch Browser"  $ rrc Apps.browser  Apps.browserClass)
  , ("M-S-e",        addName "Select or Launch Email"    $ rrc Apps.email    Apps.emailClass)

  -- NamedScratchpads
  , ("M-p",               addName "Password Manager"        $ NS.action Apps.passwordMgrClass)
  , ("M-/",               addName "Terminal Scratchpad"     $ NS.action Apps.terminalClass)
  , ("M-s",               addName "Slack Scratchpad"        $ NS.action Apps.slackClass)
  , ("M-c",               addName "Weechat Scratchpad"      $ NS.action Apps.weechatClass)
  , ("M-t",               addName "Task Scratchpad"         $ NS.action Apps.taskClass)
  , ("M-<XF86AudioPlay>", addName "Music Player Scratchpad" $ NS.action Apps.musicClass)

  -- , ("M-s s",      addName "Cancel submap" $ return ())
  -- , ("M-s M-s",    addName "Cancel submap" $ return ())
  ]

rrc a c = raiseNextMaybe (spawn a) (className c)
-- rrt a c = raiseNextMaybe (spawn a) (title     c)

className n = ManageHook.className =? n
-- title     n = ManageHook.title     =? n
