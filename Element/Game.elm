module Element.Game
    ( Action (..)
    , Game
    , TileWrapper
    , newGame
    , update
    ) where


import Debug
import Set
import List

import Chess.Chess exposing (Chess, Tile, newChess, belongsToPlayer, movePiece, possibleMoves)
import Chess.Board exposing (Sq, sqX, sqY)
import Chess.Color exposing (Color)
import Chess.Piece exposing (Piece)


type Action = NoAction | Select Sq


type alias TileWrapper =
    { sq: Sq
    , color: Color
    , piece: Piece
    , highlight: Bool
    }


type alias Game =
    { chess : Chess
    , tiles : List TileWrapper
    , selectedSq : Maybe Sq
    }


newGame : Game
newGame = refreshTiles
    { chess = newChess
    , selectedSq = Nothing
    , tiles = []
    }


update : Action -> Game -> Game
update action game =
    case action of
        NoAction ->
            game
        Select sq ->
            update' sq game


update' : Sq -> Game -> Game
update' sq game =
    let
        game' = case game.selectedSq of
            Just from ->
                move' game from sq
            Nothing ->
                selectSquare game sq
    in
       if game' == game then game else refreshTiles game'


refreshTiles : Game -> Game
refreshTiles game =
    let
        sqToCmp sq = (sqX sq, sqY sq)

        highlighted =
            game.selectedSq
                |> Maybe.map (possibleMoves game.chess)
                |> Maybe.withDefault []
                |> List.map sqToCmp
                |> Set.fromList

        mkTileWrapper tile =
            { sq = tile.sq
            , color = tile.color
            , piece = tile.piece
            , highlight = (sqToCmp tile.sq) `Set.member` highlighted
            }
    in
        { game | tiles <- List.map mkTileWrapper game.chess.tiles }




selectSquare : Game -> Sq -> Game
selectSquare game sq =
    if belongsToPlayer game.chess sq then
        { game | selectedSq <- Just sq }
    else
        game


move' : Game -> Sq -> Sq -> Game
move' game from to =
    case movePiece game.chess from to of
        Just chess' ->
            { game
                | chess <- chess'
                , selectedSq <- Nothing
                }
        Nothing ->
            { game | selectedSq <- Nothing }
