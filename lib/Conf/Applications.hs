{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses #-}
----------------------------------------------------------------------------
-- |
-- Module       : Conf.Applications
-- Copyright    : (c) maddy@na.ai
-- License      : MIT
--
-- Maintainer   : maddy@na.ai
-- Stability    : unstable
-- Portability  : unportable
--
----------------------------------------------------------------------------
module Conf.Applications where

import Text.Printf

-- import qualified GHC.IO.Handle as Handle

-- import qualified XMonad
-- import qualified XMonad.Util.Run as Run

terminal :: String
terminal = printf "termite --class='%s'" terminalClass

terminalClass :: String
terminalClass = "Termite_TERM"

terminalScratch :: String
terminalScratch = printf "termite --class='%s'" terminalScratchClass

terminalScratchClass :: String
terminalScratchClass = "Termite_SCRATCH"

editor :: String
editor = printf "termite --class='%s' -e \"zsh -c '. $HOME/.zshrc;$HOME/bin/nvim'\"" editorClass

editorClass :: String
editorClass = "Termite_EDITOR"

task :: String
task = printf "termite -c $HOME/.config/termite-task/config -d $HOME/task --class='%s' -e \"zsh -c 'export HISTFILE=.taskhistory;export ZDOTDIR=$HOME/.task;zsh'\"" taskClass

taskClass :: String
taskClass = "Termite_TASK"

weechat :: String
weechat = printf "termite -c $HOME/.config/termite-weechat/config -d $HOME --class='%s' -e \"zsh -c '. $HOME/.zshrc;weechat'\"" weechatClass

weechatClass :: String
weechatClass = "Termite_WEECHAT"

dirciple :: String
dirciple = "$HOME/bin/gds"

dircipleClass :: String
dircipleClass = "Termite_DS"

email :: String
email = "gtk-launch thunderbird"

emailClass :: String
emailClass = "Thunderbird"

slack :: String
slack = "gtk-launch slack"

slackClass :: String
slackClass = "Slack"

browser :: String
browser = "gtk-launch chromium"

browserClass :: String
browserClass = "Chromium"

music :: String
music = "$HOME/bin/mellowplayer"

musicClass :: String
musicClass = "MellowPlayer"

passwordMgr :: String
passwordMgr = "gtk-launch enpass"

passwordMgrClass :: String
passwordMgrClass = "Enpass"

launcher :: String
launcher = "rofi -matching fuzzy -modi combi -show combi -combi-modi run,drun"

statusBar :: String
statusBar = "xmobar"

statusBarConf :: String
statusBarConf = "$HOME/.xmonad/xmobar.conf"

-- statusBarPerScreen :: (Show s, Integral s) => s -> String
-- statusBarPerScreen s = "xmobar -x " ++ show s ++ " $HOME/.xmonad/xmobar.conf"
--
-- spawnStatusBar :: (Show s, Integral s, XMonad.MonadIO m) => s -> m Handle.Handle
-- spawnStatusBar s = Run.spawnPipe $ statusBarPerScreen s
--
-- spawnStatusBars :: (Show sc, Integral sc, XMonad.MonadIO m) => sc -> m [Handle.Handle]
-- spawnStatusBars sc = mapM spawnStatusBar [0..(sc - 1)]
