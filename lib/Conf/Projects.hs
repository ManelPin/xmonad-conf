{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses #-}
----------------------------------------------------------------------------
-- |
-- Module       : Conf.Projects
-- Copyright    : (c) maddy@na.ai
-- License      : MIT
--
-- Maintainer   : maddy@na.ai
-- Stability    : unstable
-- Portability  : unportable
--
----------------------------------------------------------------------------
module Conf.Projects
  ( projects
  , dynamicProjects
  ) where

import qualified Conf.Workspaces as WS

-- import qualified XMonad.Layout.IndependentScreens as IndependentScreens

-- import qualified XMonad

import XMonad.Actions.DynamicProjects

projects :: [DynamicProjects.Project]
projects =
  [ Project { projectName      = WS.foo
            , projectDirectory = "~/"
            , projectStartHook = Nothing
            }

  , Project { projectName      = WS.bar
            , projectDirectory = "~/naai/api/"
            , projectStartHook = Nothing
            }

  ]

dynamicProjects = DynamicProjects.dynamicProjects projects
