module Chess.Moves
  ( move
  , legalMoves
  , isCheck
  , isMate
  ) where


import List

import Chess.Color exposing ( Color (..), Player, otherColor )
import Chess.Piece exposing ( Piece (..), Type (..), belongsTo )
import Chess.Board exposing ( Board, Sq, squares, getPieceAt, sqAdd, movePiece, sqY)


move : Player -> Board -> Sq -> Sq -> Maybe Board
move player board from to =
    if  | to `List.member` legalMoves player board from ->
            Just (movePiece board from to)
        | otherwise ->
            Nothing


isCheck : Player -> Board -> Bool
isCheck player board =
    let
        isPlayerKing sq = case getPieceAt board sq of
          Piece c K      -> c == player
          otherwise      -> False

        opponentMoves = List.concatMap (moves opponent board) opponentSquares
        opponentSquares = List.map fst (playerPieces opponent board)
        opponent = otherColor player
    in
        List.any isPlayerKing opponentMoves


isMate : Player -> Board -> Bool
isMate player board =
    let
        possibleBoards = List.concatMap possibleBoardsFromTile playerTiles
        possibleBoardsFromTile tile = List.map (movePiece board tile) (moves player board tile)
        playerTiles = List.map fst (playerPieces player board)
    in
        (not << List.any (not << isCheck player)) possibleBoards


legalMoves : Player -> Board -> Sq -> List Sq
legalMoves player board from =
    let
        piece = getPieceAt board from
        notCheck = not << isCheck player << movePiece board from
    in
        if  | belongsTo piece player  -> List.filter notCheck (moves player board from)
            | otherwise               -> []


moves : Player -> Board -> Sq -> List Sq
moves player board from =
    case getPieceAt board from of
        None      -> []
        Piece c t -> moves' player board from t


moves' : Player -> Board -> Sq -> Type -> List Sq
moves' player board from t =
    let
        movesK' : List Sq
        movesK' =
            let
                maybeMoves = List.map maybeMove (combinations [-1 .. 1] [-1 .. 1])
                maybeMove d = mfilter (tileIsEmptyOrOpponent board player) (sqAdd from d)
            in
                catMaybes maybeMoves

        movesQ' : List Sq
        movesQ' = step board player from (straight ++ diagonal)

        movesB' : List Sq
        movesB' = step board player from diagonal

        movesN' : List Sq
        movesN' =
            let
                knightMoves' = catMaybes (List.map (sqAdd from) (List.filter (\(x, y) -> abs x + abs y == 3) (combinations [-2 .. 2] [-2 .. 2])))
            in
                List.filter (tileIsEmptyOrOpponent board player) knightMoves'

        movesR' : List Sq
        movesR' = step board player from straight

        movesP' : List Sq
        movesP' =
            let
                single = mfilter (tileIsEmpty board) (sqAdd from (0, direction))
                double =
                  if  | sqY from == start ->
                            mfilter (tileIsEmpty board) (Maybe.andThen single (\x -> sqAdd x (0, direction)))
                      | otherwise ->
                            Nothing
                captureLeft = capture (-1)
                captureRight = capture 1
                capture dx = mfilter (tileIsOpponent player board) (sqAdd from (dx, direction))
                start = case player of
                  L -> 1
                  D -> 6
                direction = case player of
                  L -> 1
                  D -> -1
            in
                catMaybes [single, double, captureLeft, captureRight]
    in
        case t of
            K -> movesK'
            Q -> movesQ'
            B -> movesB'
            N -> movesN'
            R -> movesR'
            P -> movesP'


step : Board -> Color -> Sq -> List (Int, Int) -> List Sq
step board player sq =
    let
        step' : Maybe Sq -> (Int, Int) -> List Sq
        step' maybeSq dir =
            case maybeSq of
                Nothing  ->
                    []
                Just sq' ->
                    if  | sq == sq'                       -> step' (sqAdd sq' dir) dir
                        | tileIsEmpty board sq'           -> sq'::step' (sqAdd sq' dir) dir
                        | tileIsOpponent player board sq' -> [sq']
                        | otherwise                       -> []
    in
        List.concatMap (step' (Just sq))


up        = ( 0,  1)
down      = ( 0, -1)
left      = (-1,  0)
right     = ( 1,  0)
upleft    = (-1,  1)
upright   = ( 1,  1)
downleft  = (-1, -1)
downright = ( 1, -1)
straight  = [up, down, left, right]
diagonal  = [upleft, upright, downleft, downright]


tileIsEmptyOrOpponent : Board -> Player -> Sq -> Bool
tileIsEmptyOrOpponent board player tile =
    if  | tileIsEmpty board tile            -> True
        | tileIsOpponent player board tile  -> True
        | otherwise                         -> False


tileIsEmpty : Board -> Sq -> Bool
tileIsEmpty board tile = getPieceAt board tile == None


tileIsOpponent : Player -> Board -> Sq -> Bool
tileIsOpponent player board tile = case getPieceAt board tile of
  None        -> False
  (Piece c _) -> c /= player


playerPieces : Player -> Board -> List (Sq, Piece)
playerPieces player board =
    let
        hasPlayerPiece (_, piece) =
            case piece of
                None ->
                    False
                Piece c _ ->
                    c == player
        pieces = List.map (getPieceAt board) squares
        pairs = List.map2 (,) squares pieces
    in
        List.filter hasPlayerPiece pairs


combinations : List Int -> List Int -> List (Int, Int)
combinations first second =
    let
        fst = List.concat (List.map (List.repeat (List.length second)) first)
        snd = List.concat (List.repeat (List.length first) second)
    in
       List.map2 (,) fst snd


catMaybes : List (Maybe Sq) -> List Sq
catMaybes xs =
    case xs of
        [] -> []
        Nothing::xs' -> catMaybes xs'
        (Just x)::xs' -> x::catMaybes xs'


mfilter : (a -> Bool) -> Maybe a -> Maybe a
mfilter func maybe =
    case maybe of
        Just a ->
            if func a then Just a else Nothing
        Nothing -> Nothing
