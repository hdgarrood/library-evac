module ListExtra where

(!!) : [a] -> Int -> Maybe a
xs !! i = case xs of
    (hd::tl) -> if | i < 0 -> Nothing
                   | i == 0 -> Just hd
                   | otherwise -> tl !! (i - 1)
    []       -> Nothing

safeHead : [a] -> Maybe a
safeHead xs = case xs of
    x :: _ -> Just x
    []     -> Nothing
