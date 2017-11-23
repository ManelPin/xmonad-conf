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
  -- General
  [ ("M-<Space>",   addName "Launcher"     $ spawn Apps.launcher)
  , ("M-<Return>",  addName "Terminal"     $ spawn Apps.terminal)
  , ("M-d",         addName "Dirciple"     $ spawn Apps.dirciple)
  , ("M-v",         addName "Editor"       $ spawn Apps.editor)
  , ("M-M1-v",      addName "Editor Tmpl"  $ spawn Apps.editorTmpl)
  , ("M-\\",        addName "Browser"      $ spawn Apps.browser)
  , ("M-S-\\",      addName "Alt Browser"  $ spawn Apps.altBrowser)

  -- Communications
  , ("M-s",        addName "Slack"        $ spawn Apps.slack)
  , ("M-c",        addName "Weechat"      $ spawn Apps.weechat)
  , ("M-S-c",      addName "Discord"      $ spawn Apps.discord)

  , ("M-S-e",      addName "Select or Launch Email" $ rrc Apps.email Apps.emailClass)

  -- NamedScratchpads
  , ("M-p",               addName "Enpass"                  $ NS.action Apps.enpassClass)
  , ("M-/",               addName "Terminal Scratchpad"     $ NS.action Apps.terminalClass)
  , ("M-S-f",             addName "File Manager Scratchpad" $ NS.action Apps.filemanagerClass)
  , ("M-t",               addName "Task Scratchpad"         $ NS.action Apps.taskClass)
  , ("M-<XF86AudioPlay>", addName "Music Player Scratchpad" $ NS.action Apps.musicClass)
  ]

rrc a c = raiseNextMaybe (spawn a) (className c)
className n = ManageHook.className =? n
