module Collage.Main where

import Signal
import Mouse
import List
import String
import Debug

import Collage.Draw exposing (draw, posToSq, windowCenterX, windowCenterY)
import Collage.Game exposing (Game, newGame, move)
import Chess.Util exposing (splitInto)
import Graphics.Element exposing (Element)



main : Signal Element
main =
    let
        mouseMoves = Signal.map MouseMove Mouse.position
        mouseClicks = Signal.map (\x -> MouseClick) Mouse.clicks
        shouldRedraw game = game.redraw
    in
        [mouseMoves, mouseClicks]
            |> Signal.mergeMany
            |> Signal.foldp update newGame
            |> Signal.filter shouldRedraw newGame
            |> Signal.map draw


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
