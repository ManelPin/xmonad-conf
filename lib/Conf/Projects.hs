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

import qualified Conf.Workspaces

import qualified XMonad
import qualified XMonad.Actions.DynamicProjects as DynamicProjects

projects :: [DynamicProjects.Project]
projects = []

dynamicProjects = DynamicProjects.dynamicProjects projects
