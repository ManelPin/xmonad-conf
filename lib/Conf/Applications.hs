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

import Text.Printf (printf)

terminal :: String
terminal = printf "kitty --class %s" terminalClass

terminalClass :: String
terminalClass = "kitty_TERM"

terminalScratch :: String
terminalScratch = printf "kitty --class %s" terminalScratchClass

terminalScratchClass :: String
terminalScratchClass = "kitty_SCRATCH"

editor :: String
editor = printf "kitty --class %s zsh -c '. $HOME/.zshrc;$HOME/bin/nvim'" editorClass

editorClass :: String
editorClass = "kitty_EDITOR"

task :: String
task = printf "termite -c $HOME/.config/termite-task/config -d $HOME/task --class='%s' -e \"zsh -c 'export HISTFILE=.taskhistory;export ZDOTDIR=$HOME/.task;zsh'\"" taskClass

taskClass :: String
taskClass = "Termite_TASK"

weechat :: String
weechat = printf "termite -c $HOME/.config/termite-weechat/config -d $HOME --class='%s' -e \"zsh -c '. $HOME/.zshrc;weechat'\"" weechatClass

weechatClass :: String
weechatClass = "Termite_WEECHAT"

discord :: String
discord = "discord"

discordClass :: String
discordClass = "discord"

dirciple :: String
dirciple = printf "kitty --config $HOME/.config/kitty/kitty.conf --config $HOME/.config/kitty-ds/kitty.conf --class %s zsh -c '. $HOME/.zshrc;$HOME/git/dirciple/index.js'" dircipleClass

dircipleClass :: String
dircipleClass = "kitty_DS"

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

screencast :: String
screencast = "peek"

screencastClass :: String
screencastClass = "Peek"
