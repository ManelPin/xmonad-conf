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

terminal :: String
terminal = printf "st -c %s" terminalClass

terminalClass :: String
terminalClass = "st_TERM"

terminalScratch :: String
terminalScratch = printf "st -c %s" terminalScratchClass

terminalScratchClass :: String
terminalScratchClass = "st_SCRATCH"

editor :: String
editor = printf "dst -c %s zsh -c '. $HOME/.zshrc;$HOME/bin/nvim'" editorClass

editorClass :: String
editorClass = "dst_EDITOR"

dirciple :: String
dirciple = printf "dst -c %s zsh -c '. $HOME/.zshrc;$HOME/bin/ds'" dircipleClass

dircipleClass :: String
dircipleClass = "dst_DS"

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
passwordMgr = "chromium --app=chrome-extension://khgocmkkpikpnmmkgmdnfckapcdkgfaf/popup/popup.html"

passwordMgrClass :: String
passwordMgrClass = "khgocmkkpikpnmmkgmdnfckapcdkgfaf__popup_popup.html"

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
