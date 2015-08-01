module VirtualDom.Main where

import StartApp

import String
import List

import VirtualDom.Game exposing (Action (..), newGame, update)
import VirtualDom.Draw exposing (draw)


main =
  StartApp.start { model = newGame, view = draw, update = update }
