module Chess.Board
  ( Board
  , Sq
  , sqX
  , sqY
  , squares
  , getPieceAt
  , sqAdd
  , movePiece
  , newBoard
  , sqColor
  , mkSq
  ) where


import Array
import List
import Maybe
import String
import Chess.Color exposing (Color(..))
import Chess.Piece exposing (Piece (..), Type (..), readPiece)


type Sq = Sq Int Int
type alias Idx = Int
type alias Board = Array.Array Piece


newBoard : Board
newBoard =
    newBoardStr
    |> String.words
    |> List.map readPiece
    |> List.reverse
    |> Array.fromList


newBoardStr : String
newBoardStr =
    """
    DR DN DB DK DQ DB DN DR
    DP DP DP DP DP DP DP DP
    E  E  E  E  E  E  E  E
    E  E  E  E  E  E  E  E
    E  E  E  E  E  E  E  E
    E  E  E  E  E  E  E  E
    LP LP LP LP LP LP LP LP
    LR LN LB LK LQ LB LN LR
    """


squares : List Sq
squares =
    List.map (uncurry Sq) (combinations [0..7] [0..7])


combinations : List Int -> List Int -> List (Int, Int)
combinations first second =
    let
        fst = List.concat (List.map (List.repeat (List.length second)) first)
        snd = List.concat (List.repeat (List.length first) second)
    in
       List.map2 (,) fst snd


sqAdd : Sq -> (Int, Int) -> Maybe Sq
sqAdd (Sq x y) (dx, dy) = mkSq (x + dx, y + dy)


mkSq : (Int, Int) -> Maybe Sq
mkSq (x, y) =
    if  | x >= 0 && x < 8 && y >= 0 && y < 8  -> Just (Sq x y)
        | otherwise                           -> Nothing


sqToIdx : Sq -> Idx
sqToIdx (Sq x y) = y * 8 + x


sqColor : Int -> Int -> Color
sqColor rank file =
    let
       evenRank = rank % 2 == 0
       evenFile = file % 2 == 0
    in
       if evenRank == evenFile then D else L


getPieceAt : Board -> Sq -> Piece
getPieceAt board sq =
    Maybe.withDefault None (Array.get (sqToIdx sq) board)


movePiece : Board -> Sq -> Sq -> Board
movePiece board from to =
    let
        toIdx = sqToIdx to
        fromIdx = sqToIdx from
        piece = getPieceAt board from
    in
        Array.set fromIdx None (Array.set toIdx piece board)


sqX : Sq -> Int
sqX (Sq x _) = x


sqY : Sq -> Int
sqY (Sq _ y) = y
