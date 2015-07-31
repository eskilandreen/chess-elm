module Chess.Chess
  ( Chess
  , Status (..)
  , movePiece
  , possibleMoves
  , Tile
  , newChess
  , getTileAt
  , belongsToPlayer
  ) where


import Chess.Board exposing (Board, Sq, mkSq, sqY, sqX, sqColor, newBoard, squares, getPieceAt)
import Chess.Color exposing (Color (..), otherColor)
import Chess.Moves exposing (isCheck, isMate, move, legalMoves)
import Chess.Piece exposing (Piece(..), Type(..), belongsTo)


type Status = PlayOn | Check | CheckMate

type alias Chess =
  { board  : Board
  , turn   : Color
  , status : Status
  , tiles  : List Tile
  }


type alias Tile =
    { sq: Sq
    , color: Color
    , piece: Piece
    }


newChess : Chess
newChess = newChess' L newBoard


newChess' : Color -> Board -> Chess
newChess' playerTurn board =
    { board   = board
    , turn    = playerTurn
    , status  = getStatus board playerTurn
    , tiles   = List.map (mkTile board) squares
    }


mkTile : Board -> Sq -> Tile
mkTile board sq =
    { sq = sq
    , color = (sqColor (sqX sq) (sqY sq))
    , piece = (getPieceAt board sq)
    }


getTileAt : Chess -> Sq -> Tile
getTileAt chess =
    mkTile chess.board


movePiece : Chess -> Sq -> Sq -> Maybe Chess
movePiece chess from to =
    case chess.status of
        CheckMate ->
            Nothing
        otherwise ->
            let
                newBoard = move chess.turn chess.board from to
                newPlayer = otherColor chess.turn
            in
                Maybe.map (newChess' newPlayer) newBoard


possibleMoves : Chess -> Sq -> List Sq
possibleMoves chess from =
    case chess.status of
        CheckMate ->
            []
        otherwise ->
            legalMoves chess.turn chess.board from


getStatus : Board -> Color -> Status
getStatus board color =
    if  | isMate color board  -> CheckMate
        | isCheck color board -> Check
        | otherwise           -> PlayOn


belongsToPlayer : Chess -> Sq -> Bool
belongsToPlayer chess sq =
    let
        piece = getPieceAt chess.board sq
        player = chess.turn
    in
        belongsTo piece player
