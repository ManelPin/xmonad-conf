{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable,
  TypeSynonymInstances, MultiParamTypeClasses #-}

----------------------------------------------------------------------------
-- |
-- Module       : Conf.Bindings.Keys.Commands
-- Copyright    : (c) maddy@na.ai
-- License      : MIT
--
-- Maintainer   : maddy@na.ai
-- Stability    : unstable
-- Portability  : unportable
--
----------------------------------------------------------------------------
module Conf.Bindings.Keys.Commands
  ( commands
  ) where

import Conf.Bindings.Keys.Internal (subKeys, tryMsgR)

import XMonad (X, io)
import XMonad.Actions.Commands (runCommand, defaultCommands, workspaceCommands, screenCommands)
import XMonad.Util.NamedActions (addName)

import Control.Monad ((>>=))

commands c = subKeys "Commands" c
  [ --( "M-S-/", addName "Show command menu" $ cmds >>= runCommand)
  ]


-- cmds :: X [(String, X ())]
-- cmds =
--   -- do
--     -- wsCmds <- workspaceCommands
--     -- return
--       [ --("Workspaces", wsCmds         >>= runCommand)
--        ("Screens"   , screenCommands >>= runCommand)
--       ]

-- cmds :: X [(String, X ())]
-- cmds = do
--     wscmds <- workspaceCommands
--     return $ screenCommands ++ otherCommands
--  where
--     otherCommands =
--       [ ("Screens"   , io $ (screenCommands >>= runCommand))
--       ]
