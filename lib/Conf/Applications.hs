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
-- terminal = printf "st -c %s zsh -ic 'cd $HOME;. $HOME/.zshrc'" terminalClass

terminalClass :: String
terminalClass = "st_TERM"

terminalScratch :: String
terminalScratch = printf "st -c %s" terminalScratchClass
-- terminalScratch = printf "st -c %s zsh -ic 'cd $HOME;. $HOME/.zshrc'" terminalScratchClass

terminalScratchClass :: String
terminalScratchClass = "st_SCRATCH"

editor :: String
editor = printf "st -x st.nvim -c %s zsh -c 'cd $HOME;. $HOME/.zshrc;$HOME/bin/nvim'" editorClass

editorTmpl :: String
editorTmpl = printf "st -x st.nvim -c %s zsh -c 'cd $HOME;. $HOME/.zshrc;$HOME/bin/nvim-tmpl'" editorClass

editorClass :: String
editorClass = "st_EDITOR"

dirciple :: String
dirciple = printf "st -x st.ds -c %s zsh -c 'cd $HOME;. $HOME/.zshrc;$HOME/bin/ds'" dircipleClass

dircipleClass :: String
dircipleClass = "st_DS"

task :: String
task = printf "st -x st.tw -c %s zsh -c 'cd $HOME/.task;export HISTFILE=.taskhistory;export ZDOTDIR=$HOME/.task;zsh'" taskClass

taskClass :: String
taskClass = "st_TASK"

weechat :: String
weechat = printf "st -x st.wc -c %s zsh -c 'cd $HOME;. $HOME/.zshrc;weechat'" weechatClass

weechatClass :: String
weechatClass = "st_WEECHAT"

discord :: String
discord = "discord"

discordClass :: String
discordClass = "discord"

filemanager :: String
filemanager = "gtk-launch Thunar"

filemanagerClass :: String
filemanagerClass = "Thunar"

email :: String
-- email = "gtk-launch mailspring"
email = ""

emailClass :: String
-- emailClass = "Mailspring"
emailClass = ""

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

enpass :: String
enpass = "gtk-launch enpass"

enpassClass :: String
enpassClass = "Enpass"

onepass :: String
onepass = "chromium --app=chrome-extension://khgocmkkpikpnmmkgmdnfckapcdkgfaf/popup/popup.html"

onepassWMName :: String
onepassWMName = "1Password"

onepassClass :: String
onepassClass = "khgocmkkpikpnmmkgmdnfckapcdkgfaf__popup_popup.html"

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
