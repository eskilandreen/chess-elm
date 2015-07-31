module Chess.Util
    ( splitInto
    ) where


splitInto : Int -> List a -> List (List a)
splitInto num xs =
    case xs of
        [] ->
            []
        otherwise ->
            List.take num xs :: splitInto num (List.drop num xs)

