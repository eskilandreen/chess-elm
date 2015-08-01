module Canvas.Game
    ( Game
    , newGame
    , move
    ) where


import Chess.Chess exposing (Chess, newChess, Tile, movePiece, belongsToPlayer)
import Chess.Board exposing (Sq)


type alias Game =
    { chess : Chess
    , mousePos : (Int, Int)
    , selectedSq : Maybe Sq
    , redraw : Bool
    }


newGame : Game
newGame =
    { chess = newChess
    , mousePos = (0, 0)
    , selectedSq = Nothing
    , redraw = True
    }


move : Game -> Sq -> Game
move game sq =
    case game.selectedSq of
        Just from ->
            move' game from sq
        Nothing ->
            selectSquare game sq


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
