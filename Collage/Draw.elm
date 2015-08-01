module Collage.Draw
    ( draw
    , posToSq
    , windowCenterY
    , windowCenterX
    ) where


import String
import List
import Text
import Color

import Collage.Game exposing (Game)
import Chess.Chess exposing (Tile, Status (..), possibleMoves)
import Chess.Piece exposing (Piece (..))
import Chess.Board exposing (Sq, sqX, sqY, mkSq)
import Chess.Color exposing (Color (..))

import Graphics.Element exposing (Element, flow, down, show, image, flow, down, right, leftAligned, rightAligned, width)
import Graphics.Collage exposing (Form, collage, toForm, move, square, defaultLine, outlined)




tileHeight : Float
tileHeight = 40

tileWidth : Float
tileWidth = 40

windowHeight = 8 * tileHeight
windowWidth = 8 * tileWidth

windowCenterY : Float
windowCenterY = windowHeight / 2

windowCenterX : Float
windowCenterX = windowWidth / 2


draw : Game -> Element
draw game =
    flow down
        [ drawBoard game
        , drawStatusBar game
        ]


drawStatusBar : Game -> Element
drawStatusBar game =
    let
        turn = if game.chess.turn == L then "White's turn" else "Black's turn"
        status = toString game.chess.status
        texts =
            [ (width 160 << leftAligned << Text.fromString) turn
            , (width 160 << rightAligned << Text.fromString) status
            ]
    in
        flow right texts


drawBoard : Game -> Element
drawBoard game =
    let
        tiles = List.map drawTile game.chess.tiles
        moves = drawMoves game
    in
        collage (8*40) (8*40) (tiles ++ moves)


drawMoves : Game -> List Form
drawMoves game =
    case game.selectedSq of
        Nothing ->
            []
        Just sq ->
            let
                moves = possibleMoves game.chess sq
                line = { defaultLine
                            | width <- 2
                            , color <- Color.red
                            }
                thickRectangleWire sq =
                    square 40
                        |> outlined line
                        |> move (sqToPos sq)

            in
                List.map thickRectangleWire moves


drawTile : Tile -> Form
drawTile tile =
    filename tile.color tile.piece
        |> image 40 40
        |> toForm
        |> move (sqToPos tile.sq)


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


posToSq : (Int, Int) -> Maybe Sq
posToSq (x, y) =
    let
        x' = (toFloat x + windowCenterX - tileWidth / 2) / tileWidth
        y' = (toFloat y + windowCenterY - tileHeight / 2) / tileHeight
    in
        mkSq (round x', round y')


sqToPos : Sq -> (Float, Float)
sqToPos sq =
    let
        x = toFloat (sqX sq)
        y = toFloat (sqY sq)

        y' = (y * tileHeight) + tileHeight / 2 - windowCenterY
        x' = (x * tileWidth) + tileWidth / 2 - windowCenterX
    in
        (x', y')
