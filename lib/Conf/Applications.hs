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
dirciple = printf "st -x st.ds -c %s zsh -c 'cd $HOME;. $HOME/.zshrc;TERM=xterm-256color $HOME/bin/ds'" dircipleClass

dircipleClass :: String
dircipleClass = "st_DS"

task :: String
task = printf "st -x st.tw -c %s zsh -c 'cd $HOME/.task;export HISTFILE=.taskhistory;export ZDOTDIR=$HOME/.task;zsh'" taskClass

taskClass :: String
taskClass = "st_TASK"

pom :: String
pom = printf "st -x st.pom -c %s zsh -c 'cd $HOME/.pom;export HISTFILE=.pomhistory;export ZDOTDIR=$HOME/.pom;zsh'" pomClass

pomClass :: String
pomClass = "st_pom"

weechat :: String
weechat = printf cmd weechatClass
  where
    cmd =  "st -x st.wc -c %s zsh -c 'cd $HOME;. $HOME/.zshrc;"
        ++ "export DISABLE_AUTO_TITLE='true'; echo -en \"\\e]2;WeeChat\\a\"; weechat'"

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

cliFilemanager :: String
cliFilemanager = printf "st -x st.rngr -c %s zsh -c 'cd $HOME;. $HOME/.zshrc;/usr/bin/ranger'" cliFilemanagerClass

cliFilemanagerClass :: String
cliFilemanagerClass = "st_RANGER"

calc :: String
calc = printf "st -x st.calc -c %s zsh -c 'cd $HOME/.calc;. $HOME/.zshrc;R'" calcClass

calcClass :: String
calcClass = "st_CALC"

_man :: String -> String
_man c = printf "st -x st.man -c %s zsh -c 'cd $HOME;. $HOME/.zshrc;manloop'" c

man :: String
man = _man manClass

manScratch :: String
manScratch = _man manScratchClass

manClass :: String
manClass = "st_MAN"

manScratchClass :: String
manScratchClass = "st_MAN_SCRATCH"

_email :: String -> String
_email c = printf "st -x st.mutt -c %s zsh -c 'cd $HOME;. $HOME/.zshrc;$HOME/bin/mutt'" c

email :: String
email = _email emailClass

emailScratch :: String
emailScratch = _email emailScratchClass

emailClass :: String
emailClass = "st_MUTT"

emailScratchClass :: String
emailScratchClass = "st_MUTT_SCRATCH"

slack :: String
slack = "gtk-launch slack"

slackClass :: String
slackClass = "Slack"

browser :: String
browser = "gtk-launch chromium"

browserClass :: String
browserClass = "Chromium"

browserScratch :: String
browserScratch = "chromium --user-data-dir=$HOME/.config/chromium-scratch --class=Chromium_SCRATCH"

browserScratchClass :: String
browserScratchClass = "Chromium_SCRATCH"

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

qrGen :: String
qrGen = "xcqr"

volumeControl :: String
volumeControl = "pavucontrol"

volumeControlClass :: String
volumeControlClass = "Pavucontrol"

audioEqualizer :: String
audioEqualizer = "qpaeq"

audioEqualizerClass :: String
audioEqualizerClass = "qpaeq"

dpyctl :: String
dpyctl = "dpyctl"
