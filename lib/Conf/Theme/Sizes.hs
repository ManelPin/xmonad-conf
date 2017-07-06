{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable,
  TypeSynonymInstances, MultiParamTypeClasses #-}

----------------------------------------------------------------------------
-- |
-- Module       : Conf.Theme.Sizes
-- Copyright    : (c) maddy@na.ai
-- License      : MIT
--
-- Maintainer   : maddy@na.ai
-- Stability    : unstable
-- Portability  : unportable
--
----------------------------------------------------------------------------
module Conf.Theme.Sizes where

import qualified Data.Word as Word

gap:: Int
gap = 20

gapSmall :: Int
gapSmall = 10

gapBig :: Int
gapBig = 30

topbar :: Word.Word32
topbar = 20

border :: Word.Word32
border = 0

prompt :: Word.Word32
prompt = 50

status :: Word.Word32
status = 20
