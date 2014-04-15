module DictExtra where

import Dict (Dict, insert, foldl, empty)

{-| Keep only key-value pairs where the value satisfies a predicate. -}
filter : (v -> Bool) -> Dict comparable v -> Dict comparable v
filter p = filterWithKey (\_ v -> p v)

{-| Keep only key-value pairs which satisfy a predicate. -}
filterWithKey : (comparable -> v -> Bool)
              -> Dict comparable v
              -> Dict comparable v
filterWithKey p =
  let add k v t = if p k v then insert k v t else t
  in foldl add empty

{-| Partition the Dict according to a predicate. The first Dict contains all
values which satisfy a predicate; the second contains the rest. -}
partition : (v -> Bool)
          -> Dict comparable v
          -> (Dict comparable v, Dict comparable v)
partition p = partitionWithKey (\_ v -> p v)

{-| Partition the Dict according to a predicate. The first Dict contains all
key-value pairs which satisfy a predicate; the second contains the rest. -}
partitionWithKey : (comparable -> v -> Bool)
                 -> Dict comparable v
                 -> (Dict comparable v, Dict comparable v)
partitionWithKey p =
  let add k v (t1, t2) = if p k v
                            then (insert k v t1,  t2)
                            else (t1, insert k v t2)
  in foldl add (empty, empty)
