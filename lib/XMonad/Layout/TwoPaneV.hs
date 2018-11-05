{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.TwoPaneV
-- Copyright   :  (c) Spencer Janssen <spencerjanssen@gmail.com>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Spencer Janssen <spencerjanssen@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A layout that splits the screen vertically and shows two windows.  The
-- top window is always the master window, and the bottom is either the
-- currently focused window or the second window in layout order.
--
-----------------------------------------------------------------------------

module XMonad.Layout.TwoPaneV (
                              -- * Usage
                              -- $usage
                              TwoPaneV (..)
                             ) where

import XMonad hiding (focus)
import XMonad.StackSet ( focus, up, down)

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.TwoPaneV
--
-- Then edit your @layoutHook@ by adding the TwoPaneV layout:
--
-- > myLayout = TwoPaneV (3/100) (1/2)  ||| Full ||| etc..
-- > main = xmonad def { layoutHook = myLayout }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

data TwoPaneV a =
    TwoPaneV Rational Rational
    deriving ( Show, Read )

instance LayoutClass TwoPaneV a where
    doLayout (TwoPaneV _ split) r s = return (arrange r s,Nothing)
        where
          arrange rect st = case reverse (up st) of
                              (master:_) -> [(master,top),(focus st,bottom)]
                              [] -> case down st of
                                      (next:_) -> [(focus st,top),(next,bottom)]
                                      [] -> [(focus st, rect)]
              where (top, bottom) = splitVerticallyBy split rect

    handleMessage (TwoPaneV delta split) x =
        return $ case fromMessage x of
                   Just Shrink -> Just (TwoPaneV delta (split - delta))
                   Just Expand -> Just (TwoPaneV delta (split + delta))
                   _           -> Nothing

    description _ = "TwoPaneV"
