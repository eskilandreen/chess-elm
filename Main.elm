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
        Signal.map draw foldp


shouldRedraw : Update -> Bool
shouldRedraw update =
    case update of
        MouseMove _ -> False
        MouseClick -> True


type Update
    = MouseMove (Int, Int)
    | MouseClick


update : Update -> Game -> Game
update update game =
    case update of
        MouseMove (x, y) ->
            { game | mousePos <- (x - round windowCenterX, round windowCenterY - y) }
        MouseClick ->
            Debug.log "Waargth!" (posToSq game.mousePos)
                |> Maybe.map (move game)
                |> Maybe.withDefault game


