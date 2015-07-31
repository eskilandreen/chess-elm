import Signal
import Mouse
import List
import String
import Debug

import Graphics.Element exposing (Element)
import Chess.Util exposing (splitInto)
import Chess.Draw exposing (draw, posToSq, windowCenterX, windowCenterY)
import Chess.Game exposing (Game, newGame, move)


main : Signal Element
main =
    let
        mouseMove = Signal.map MouseMove Mouse.position
        mouseClick = Signal.map (\x -> MouseClick) Mouse.clicks
        signals = Signal.mergeMany
            [ mouseMove
            , mouseClick
            ]
        foldp = Signal.foldp update newGame signals
    in
        Signal.map draw (Signal.filter shouldRedraw newGame foldp)


shouldRedraw : Game -> Bool
shouldRedraw game =
    game.redraw


type Update
    = MouseMove (Int, Int)
    | MouseClick


update : Update -> Game -> Game
update update game =
    let
        game' = { game | redraw <- True }
    in
        case update of
            MouseMove (x, y) ->
                { game'
                    | mousePos <- (x - round windowCenterX, round windowCenterY - y)
                    , redraw <- False
                }
            MouseClick ->
                posToSq game.mousePos
                    |> Maybe.map (move game')
                    |> Maybe.withDefault game'
