module VirtualDom.Draw (draw) where

import List
import String
import Signal exposing (Address)

import Chess.Board exposing (sqX, sqY)
import Chess.Chess exposing (Chess)
import Chess.Color exposing (Color (..))
import Chess.Piece exposing (Piece (..))
import Chess.Util exposing (splitInto)

import Html exposing (Html, div, button, text, table, tr, td, img, span)
import Html.Attributes exposing (src, style, colspan)
import Html.Events exposing (onClick)

import VirtualDom.Game exposing (Game, TileWrapper, Action(..))


draw address game =
  table [] (drawBoard address game ++ [drawStatusBar game])


drawStatusBar : Game -> Html
drawStatusBar game =
    let
        turn = if game.chess.turn == L then "White's turn" else "Black's turn"
        status = toString game.chess.status
        align dir = style [("text-align", dir)]
    in
        tr []
            [ td [colspan 4, align "left"] [text turn]
            , td [colspan 4, align "right"] [text status]
            ]


drawBoard : Address Action -> Game -> List Html
drawBoard address game =
    let
        cmp tile = (sqY tile.sq, sqX tile.sq)
    in
        game.tiles
            |> List.sortBy cmp
            |> splitInto 8
            |> List.reverse
            |> List.map (drawRow address)


drawRow : Address Action -> List TileWrapper -> Html
drawRow address tiles =
    tiles
        |> List.map (drawTile address)
        |> tr []


drawTile : Address Action -> TileWrapper -> Html
drawTile address tile =
    let
        fname = filename tile.color tile.piece
        tdstyle = if tile.highlight then [("background-color", "red")] else []
        imgstyle = [("display", "block"), ("margin", "1px")]
        action = Select tile.sq
    in
        td [style tdstyle]
            [img [style imgstyle, src fname, onClick address action] []]


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
