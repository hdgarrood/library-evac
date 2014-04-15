module Extra where

import Types (..)

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

(!!) : [a] -> Int -> Maybe a
xs !! i = case xs of
    (hd::tl) -> if | i < 0 -> Nothing
                   | i == 0 -> Just hd
                   | otherwise -> tl !! (i - 1)
    []       -> Nothing
