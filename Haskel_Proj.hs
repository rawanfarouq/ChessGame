
type Location = (Char, Int)

data Player = White | Black deriving (Show, Eq)

data Piece = P Location | N Location | K Location | Q Location | R Location | B Location deriving (Show, Eq)

type Board = (Player, [Piece], [Piece])

initBoard :: Board
initBoard = (White, whitePieces, blackPieces)
  where
    whitePieces = [R ('a', 1), N ('b', 1), B ('c', 1), Q ('d', 1), K ('e', 1), B ('f', 1), N ('g', 1), R ('h', 1)]
                  ++ map (\c -> P (c, 2)) ['a'..'h']
    blackPieces = [R ('a', 8), N ('b', 8), B ('c', 8), Q ('d', 8), K ('e', 8), B ('f', 8), N ('g', 8), R ('h', 8)]
                  ++ map (\c -> P (c, 7)) ['a'..'h']

setBoard :: Board
setBoard = initBoard

---------------------------------------------------------------------------------------

visualizeBoard :: Board -> String
visualizeBoard (turn, whitePieces, blackPieces) = boardString ++ "\nTurn: " ++ show turn
  where
    boardString = unlines (rowLabels ++ [columnLabels] ++ boardRows)
    rowLabels = map (\row -> show row ++ " |") [8,7..1]
    columnLabels = "  " ++ unwords (map (\col -> " " ++ [col] ++ " ") ['a'..'h'])
    boardRows = [rowToString row | row <- [8,7..1]]

    rowToString :: Int -> String
    rowToString row = show row ++ " |" ++ concat [pieceToString (col, row) ++ " |" | col <- ['a'..'h']]

    pieceToString :: Location -> String
    pieceToString loc
      | pieceInList whitePieces loc = pieceToString' "W"
      | pieceInList blackPieces loc = pieceToString' "B"
      | otherwise = " "
      where
        pieceToString' suffix = case loc of
          ('a', 1) -> "R" ++ suffix
          ('b', 1) -> "N" ++ suffix
          ('c', 1) -> "B" ++ suffix
          ('d', 1) -> "Q" ++ suffix
          ('e', 1) -> "K" ++ suffix
          ('f', 1) -> "B" ++ suffix
          ('g', 1) -> "N" ++ suffix
          ('h', 1) -> "R" ++ suffix
          ('a', 2) -> "P" ++ suffix
          ('b', 2) -> "P" ++ suffix
          ('c', 2) -> "P" ++ suffix
          ('d', 2) -> "P" ++ suffix
          ('e', 2) -> "P" ++ suffix
          ('f', 2) -> "P" ++ suffix
          ('g', 2) -> "P" ++ suffix
          ('h', 2) -> "P" ++ suffix
          _ -> " "

    pieceInList :: [Piece] -> Location -> Bool
    pieceInList pieces loc' = any (\piece -> case piece of
                                             P l -> l == loc'
                                             N l -> l == loc'
                                             K l -> l == loc'
                                             Q l -> l == loc'
                                             R l -> l == loc'
                                             B l -> l == loc') pieces
											 
											 
											 
-------------------------------------------------------------------------------------------------------------------

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing  = False

indexToColumn :: Int -> Char
indexToColumn index = toEnum (fromEnum 'a' + index - 1)

find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find p (x:xs)
  | p x       = Just x
  | otherwise = find p xs

isWhitePieceAtLocation :: Location -> [Piece] -> Bool
isWhitePieceAtLocation loc pieces = any (\piece -> isWhitePiece piece && extractLocation piece == loc) pieces
  where
    isWhitePiece (P _) = True
    isWhitePiece (N _) = True
    isWhitePiece (K _) = True
    isWhitePiece (Q _) = True
    isWhitePiece (R _) = True
    isWhitePiece (B _) = True
    isWhitePiece _     = False

extractLocation :: Piece -> Location
extractLocation (P loc) = loc
extractLocation (N loc) = loc
extractLocation (K loc) = loc
extractLocation (Q loc) = loc
extractLocation (R loc) = loc
extractLocation (B loc) = loc

isOccupiedByPiece :: [Piece] -> Location -> Bool
isOccupiedByPiece pieces loc = any (\piece -> extractLocation piece == loc) pieces

isPathClearRook :: Location -> Location -> [Piece] -> Bool
isPathClearRook (col1, row1) (col2, row2) pieces =
  let colDiff = abs (columnToIndex col2 - columnToIndex col1)
      rowDiff = abs (row2 - row1)
      pathLocations = [(col1, row1 + d) | d <- [1..rowDiff-1]]
                   ++ [(indexToColumn (columnToIndex col1 + d), row1) | d <- [1..colDiff-1]]
  in all (\loc -> not (isOccupiedByPiece pieces loc)) pathLocations

isPathClearBishop :: Location -> Location -> [Piece] -> Bool
isPathClearBishop (col1, row1) (col2, row2) pieces =
  let colDiff = abs (columnToIndex col2 - columnToIndex col1)
      rowDiff = abs (row2 - row1)
      pathLocations = [(indexToColumn (columnToIndex col1 + d), row1 + d) | d <- [1..rowDiff-1]]
                   ++ [(indexToColumn (columnToIndex col1 + d), row1 - d) | d <- [1..rowDiff-1]]
  in all (\loc -> not (isOccupiedByPiece pieces loc)) pathLocations

columnToIndex :: Char -> Int
columnToIndex col = fromEnum col - fromEnum 'a' + 1

isLegal :: Piece -> Board -> Location -> Bool
isLegal (P loc) (_, whitePieces, blackPieces) destLoc =
  let opponentPieces = if isWhitePieceAtLocation loc whitePieces then blackPieces else whitePieces
  in not (isOccupiedByPiece whitePieces destLoc || isOccupiedByPiece blackPieces destLoc)
isLegal _ _ _ = False


isLegalPawnMove :: Piece -> Board -> Location -> Bool
isLegalPawnMove (P (col, row)) (turn, whitePieces, blackPieces) destLoc@(destCol, destRow) =
  let direction = if turn == White then 1 else -1
      validMove = case turn of
        White -> (destRow - row) == 1 && abs (columnToIndex col - columnToIndex destCol) <= 1
        Black -> (row - destRow) == 1 && abs (columnToIndex col - columnToIndex destCol) <= 1
      validCapture = case turn of
        White -> (destRow - row) == 1 && abs (columnToIndex col - columnToIndex destCol) == 1
        Black -> (row - destRow) == 1 && abs (columnToIndex col - columnToIndex destCol) == 1
      destinationPiece = getPieceAtLocation destLoc whitePieces blackPieces
  in validMove && isFreeLocation whitePieces blackPieces destLoc && (validCapture || isJust destinationPiece)

isLegalKnightMove :: Piece -> Board -> Location -> Bool
isLegalKnightMove (N (col, row)) (turn, whitePieces, blackPieces) destLoc@(destCol, destRow) =
  let validMove = (abs (columnToIndex col - columnToIndex destCol) == 2 && abs (row - destRow) == 1) ||
                  (abs (columnToIndex col - columnToIndex destCol) == 1 && abs (row - destRow) == 2)
      destinationPiece = getPieceAtLocation destLoc whitePieces blackPieces
  in validMove && (isFreeLocation whitePieces blackPieces destLoc || isJust destinationPiece)

isLegalKingMove :: Piece -> Board -> Location -> Bool
isLegalKingMove (K (col, row)) (turn, whitePieces, blackPieces) destLoc@(destCol, destRow) =
  let validMove = abs (columnToIndex col - columnToIndex destCol) <= 1 && abs (row - destRow) <= 1
      destinationPiece = getPieceAtLocation destLoc whitePieces blackPieces
  in validMove && (isFreeLocation whitePieces blackPieces destLoc || isJust destinationPiece)

isLegalQueenMove :: Piece -> Board -> Location -> Bool
isLegalQueenMove (Q (col, row)) board destLoc =
  isLegalRookMove (R (col, row)) board destLoc || isLegalBishopMove (B (col, row)) board destLoc

isLegalRookMove :: Piece -> Board -> Location -> Bool
isLegalRookMove (R (col, row)) (turn, whitePieces, blackPieces) destLoc@(destCol, destRow) =
  let validMove = col == destCol || row == destRow
      destinationPiece = getPieceAtLocation destLoc whitePieces blackPieces
  in validMove && isPathClearRook (col, row) destLoc whitePieces blackPieces && (isFreeLocation whitePieces blackPieces destLoc || isJust destinationPiece)

isLegalBishopMove :: Piece -> Board -> Location -> Bool
isLegalBishopMove (B (col, row)) (turn, whitePieces, blackPieces) destLoc@(destCol, destRow) =
  let validMove = abs (columnToIndex col - columnToIndex destCol) == abs (row - destRow)
      destinationPiece = getPieceAtLocation destLoc whitePieces blackPieces
  in validMove && isPathClearBishop (col, row) destLoc whitePieces blackPieces && (isFreeLocation whitePieces blackPieces destLoc || isJust destinationPiece)



getPieceAtLocation :: Location -> [Piece] -> [Piece] -> Maybe Piece
getPieceAtLocation loc whitePieces blackPieces =
  let pieces = if isWhitePieceAtLocation loc whitePieces then whitePieces else blackPieces
  in find (\piece -> extractLocation piece == loc) pieces

isFreeLocation :: [Piece] -> [Piece] -> Location -> Bool
isFreeLocation whitePieces blackPieces loc =
  not (isOccupiedByPiece whitePieces loc || isOccupiedByPiece blackPieces loc)


						 
											 