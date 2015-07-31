module Chess.Util
    ( splitInto
    , combinations
    , catMaybes
    , mfilter
    ) where


import List


-- Splits a list into parts of `num` elements. If the last part is shorter than
-- `num` then it will contain whatever was left.
splitInto : Int -> List a -> List (List a)
splitInto num xs =
    case xs of
        [] ->
            []
        otherwise ->
            List.take num xs :: splitInto num (List.drop num xs)

-- Returns all pairs created using one element from the first list and one
-- element from the second.
combinations : List a -> List a -> List (a, a)
combinations first second =
    let
        fst = List.concat (List.map (List.repeat (List.length second)) first)
        snd = List.concat (List.repeat (List.length first) second)
    in
       List.map2 (,) fst snd


-- Removes all Nothings and returns the values of the remaining Justs.
catMaybes : List (Maybe a) -> List a
catMaybes xs =
    case xs of
        [] -> []
        Nothing::xs' -> catMaybes xs'
        (Just x)::xs' -> x::catMaybes xs'


-- mfilter (\x -> True) (Just 7) == Just 7
-- mfilter (\x -> False) (Just 7) == Nothing
-- mfilter _ Nothing == Nothing
mfilter : (a -> Bool) -> Maybe a -> Maybe a
mfilter func maybe =
    case maybe of
        Just a ->
            if func a then Just a else Nothing
        Nothing -> Nothing
