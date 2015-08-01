module Element.Main where

import List
import Signal
import String

import Element.Game exposing (Action (..), newGame, update)
import Element.Draw exposing (draw)

import Graphics.Element exposing (Element)


mailbox : Signal.Mailbox Action
mailbox = Signal.mailbox NoAction


main : Signal Element
main =
    mailbox.signal
        |> Signal.foldp update newGame
        |> Signal.map (draw mailbox.address)
