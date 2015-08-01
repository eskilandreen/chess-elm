module Element.Draw (draw) where

import Color
import List
import String
import Signal
import Text

import Chess.Board exposing (sqX, sqY)
import Chess.Chess exposing (Chess)
import Chess.Color exposing (Color (..))
import Chess.Piece exposing (Piece (..))
import Chess.Util exposing (splitInto)

import Graphics.Element exposing (Element, flow, down, right, image, color, leftAligned, rightAligned, container, middle, width, widthOf)
import Graphics.Input exposing (clickable)

import Element.Game exposing (Game, TileWrapper, Action(..))


draw address game =
    let
        board = drawBoard address game
        bar = drawStatusBar (widthOf board) game
    in
        flow down [board, bar]


drawStatusBar : Int -> Game -> Element
drawStatusBar barWidth game =
    let
        turn = if game.chess.turn == L then "White's turn" else "Black's turn"
        status = toString game.chess.status
        texts = [ leftAligned (Text.fromString turn)
                , rightAligned (Text.fromString status)
                ]
        textWidth = barWidth // List.length texts
    in
        texts
            |> List.map (width textWidth)
            |> flow right


drawBoard : Signal.Address Action -> Game -> Element
drawBoard address game =
    let
        cmp tile = (sqY tile.sq, sqX tile.sq)
    in
        game.tiles
            |> List.sortBy cmp
            |> splitInto 8
            |> List.reverse
            |> List.map (drawRow address)
            |> flow down


drawRow : Signal.Address Action -> List TileWrapper -> Element
drawRow address tiles =
    tiles
        |> List.map (drawTile address)
        |> flow right


drawTile : Signal.Address Action -> TileWrapper -> Element
drawTile address tile =
    let
        fname = filename tile.color tile.piece
        action = Select tile.sq
        c = if tile.highlight then Color.red else Color.white
    in
        image 40 40 fname
        |> clickable (Signal.message address action)
        |> container 44 44 middle
        |> color c
        |> container 46 46 middle


filename : Color -> Piece -> String
filename color piece =
    let
        colorEnc = toString color
        pieceEnc =
            case piece of
                None -> ""
                Piece c t -> (toString t) ++ (toString c)
        enc = String.toLower (pieceEnc ++ colorEnc)
    in
        "https://raw.githubusercontent.com/mtak/chess-elm/master/pieces/Chess_" ++ enc ++ "40.png"
