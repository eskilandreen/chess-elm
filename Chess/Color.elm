module Chess.Color
  ( Color (..)
  , Player
  , otherColor
  , readColor
  ) where


type Color = L | D
type alias Player = Color


otherColor : Color -> Color
otherColor c =
    case c of
        L -> D
        D -> L


readColor : Char -> Color
readColor c =
    case c of
        'L' -> L
        'D' -> D
