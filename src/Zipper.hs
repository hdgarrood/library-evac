module Zipper where

import Types

advance :: Zipper a -> Maybe (Zipper a)
advance (Z preds x succs) = case succs of
    y : ys -> Just $ Z (x : preds) y ys
    []     -> Nothing

retreat :: Zipper a -> Maybe (Zipper a)
retreat (Z preds x succs) = case preds of
    y : ys -> Just $ Z ys y (x : succs)
    []     -> Nothing

current :: Zipper a -> a
current (Z _ x _) = x

setCurrent :: Zipper a -> a -> Zipper a
setCurrent (Z preds _ succs) z = Z preds z succs

values :: Zipper a -> [a]
values (Z preds x succs) = x : preds ++ succs
