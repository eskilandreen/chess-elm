module Chess.Piece
  ( Type (..)
  , Piece (..)
  , belongsTo
  , readPiece
  ) where

import String
import Chess.Color exposing (Color (..), Player, readColor)


type Type = K | Q | R | B | N | P


type Piece
  = None
  | Piece Color Type


belongsTo : Piece -> Player -> Bool
belongsTo piece player =
    case piece of
        Piece color _ ->
            color == player
        otherwise ->
            False



readPiece : String -> Piece
readPiece str =
    case String.toList str of
        ['E'] -> None
        [c, t] -> Piece (readColor c) (readType t)


readType : Char -> Type
readType c =
    case c of
        'K' -> K
        'Q' -> Q
        'R' -> R
        'B' -> B
        'N' -> N
        'P' -> P
