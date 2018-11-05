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
  , ("M-S-m",       addName "Manpage"      $ spawn Apps.man)

  -- Communications
  , ("M-s",        addName "Slack"        $ spawn Apps.slack)
  , ("M-c",        addName "Weechat"      $ spawn Apps.weechat)
  , ("M-S-c",      addName "Discord"      $ spawn Apps.discord)

  -- Run or Raise
  , ("M-S-d",        addName "Select or Launch Dirciple" $ rrc Apps.dirciple Apps.dircipleClass)
  , ("M-S-v",        addName "Select or Launch Editor"   $ rrc Apps.editor   Apps.editorClass)
  , ("M-S-<Return>", addName "Select or Launch Terminal" $ rrc Apps.terminal Apps.terminalClass)
  , ("M-S-e",        addName "Select or Launch Email"    $ rrc Apps.email    Apps.emailClass)

  -- NamedScratchpads
  , ("M-e",               addName "Email Scratchpad"            $ NS.action Apps.emailScratchClass)
  , ("M-p",               addName "Enpass Scratchpad"           $ NS.action Apps.enpassClass)
  , ("M-S-p",             addName "Pomodoro Scratchpad"         $ NS.action Apps.pomClass)
  , ("M-,",               addName "Calculator Scratchpad"       $ NS.action Apps.calcClass)
  , ("M-/",               addName "Terminal Scratchpad"         $ NS.action Apps.terminalClass)
  , ("M-m",               addName "Manpage Scratchpad"          $ NS.action Apps.manScratchClass)
  , ("M-S-\\",            addName "Browser Scratchpad"          $ NS.action Apps.browserScratchClass)
  , ("M-.",               addName "CLI File Manager Scratchpad" $ NS.action Apps.cliFilemanagerClass)
  , ("M-S-f",             addName "File Manager Scratchpad"     $ NS.action Apps.filemanagerClass)
  , ("M-t",               addName "Task Scratchpad"             $ NS.action Apps.taskClass)

  , ("M-<XF86AudioPlay>",   addName "Music Player Scratchpad"     $ NS.action Apps.musicClass)
  , ("M-<XF86AudioMute>",   addName "Volume Control Scratchpad"   $ NS.action Apps.volumeControlClass)
  , ("M-S-<XF86AudioMute>", addName "Audio Eqalizer Scratchpad"   $ NS.action Apps.audioEqualizerClass)

  -- Clipboard
  , ( "M-<F4>", addName "Generate QR Code from Clipboard contents" $ spawn Apps.qrGen)
  , ( "M-<F5>", addName "View image from path in Clipboard contents" $ spawn Apps.clipView)
  ]

className n = ManageHook.className =? n
rrc a c = raiseNextMaybe (spawn a) (className c)
