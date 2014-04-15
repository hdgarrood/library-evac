module Zipper where

data Zipper a = Z [a] a [a]

advance : Zipper a -> Maybe (Zipper a)
advance (Z preds x succs) = case succs of
    hd::tl -> Just <| Z (x :: preds) hd tl
    []     -> Nothing

retreat : Zipper a -> Maybe (Zipper a)
retreat (Z preds x succs) = case preds of
    hd::tl -> Just <| Z tl hd (x :: succs)
    []     -> Nothing

current : Zipper a -> a
current (Z _ x _) = x

setCurrent : Zipper a -> a -> Zipper a
setCurrent (Z preds _ succs) z = Z preds z succs

zmap : (a -> b) -> Zipper a -> Zipper b
zmap f (Z preds x succs) = Z (map f preds) (f x) (map f succs)

values : Zipper a -> [a]
values (Z preds x succs) = x :: preds ++ succs
