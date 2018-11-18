{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Row
-- Copyright   :  (c) 2018 Maddison Hellstrom <maddy@na.ai>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Maddison Hellstrom <maddy@na.ai>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Provides Row layout that places all windows in one row. Windows
-- widths are calculated from equation: H1/H2 = H2/H3 = ... = q, where q is
-- given. With Shrink/Expand messages you can change the q value.
--
-----------------------------------------------------------------------------

module XMonad.Layout.Row (
                             -- * Usage
                             -- $usage
                             Row (..)
                            ) where
import XMonad
import qualified XMonad.StackSet as W

-- $usage
-- This module defines layot named Row. It places all windows in one
-- row. Windows widths are calculated from equation: W1/W2 = W2/W3 = ... =
-- q, where `q' is given (thus, windows widths are members of geometric
-- progression). With Shrink/Expand messages one can change the `q' value.
--
-- You can use this module by adding folowing in your @xmonad.hs@:
--
-- > import XMonad.Layout.Row
--
-- Then add layouts to your layoutHook:
--
-- > myLayoutHook = Row 1.6 ||| ...
--
-- In this example, each next window will have width 1.6 times less then
-- previous window.

data Row a = Row Float deriving (Read,Show)

instance LayoutClass Row a where
    pureLayout = rowLayout
    pureMessage = rowMessage

rowMessage :: Row a -> SomeMessage -> Maybe (Row a)
rowMessage (Row q) m = fmap resize (fromMessage m)
    where resize Shrink = Row (q-0.1)
          resize Expand = Row (q+0.1)

rowLayout :: Row a -> Rectangle -> W.Stack a -> [(a,Rectangle)]
rowLayout (Row q) rect stack = zip ws rects
    where ws = W.integrate stack
          n = length ws
          widths = map (xn n rect q) [1..n]
          xs = [fromIntegral $ sum $ take k widths | k <- [0..n-1]]
          rects = map (mkRect rect) $ zip widths xs

mkRect :: Rectangle -> (Dimension,Position) -> Rectangle
mkRect (Rectangle xs ys _ hs) (w,x) = Rectangle (xs+fromIntegral x) ys w hs

xn :: Int -> Rectangle -> Float -> Int -> Dimension
xn n (Rectangle _ _ w _) q k = if q==1 then
                                  w `div` (fromIntegral n)
                               else
                                  round ((fromIntegral w)*q^(n-k)*(1-q)/(1-q^n))
