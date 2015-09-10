module Board where

import qualified Data.Map as M
import qualified Data.List as L

data Cell = Black
          | White
          | Empty
          deriving(Show, Eq)

data Board = Board {
  posMap :: M.Map (Int, Int) Cell,
  size :: Int
  }

instance Show Board where
  show = unlines . map concat . L.transpose . makeCols

makeBoard :: Int -> Board
makeBoard n =
  let
    coordMax = (n-1) * 2
    grid =
      M.fromList $ map (\p -> (p, Empty))
      (filter
       (\(x, y) -> (x >= 0 && x <= coordMax &&
                   y >= 0 && y <= coordMax &&
                   abs (x-y) < n))
       [(x, y) | x <- [0..coordMax], y <- [0..coordMax]])
  in Board grid n

diam :: Board -> Int
diam = (+ (-1)) . (* 2) . size

makeSepCol :: Board -> Int -> [String]
makeSepCol b i =
  let numSep = if i < size b then i + size b else 3 * size b - 1 - i
      pad = replicate (2*(2 * size b - 1 - numSep)) "  "
      leftSep = [" /", "/ ", "\\ ", " \\"]
      rightSep = ["\\ ", " \\", " /", "/ "]
      sep = if i < size b then leftSep else rightSep
      line = "  " : "  " : pad ++ concat (replicate numSep sep) ++ pad
  in
   line

makeCellCol :: Board -> Int -> [String]
makeCellCol b i =
  let targetDiff = size b - 1 - i
      cells = M.filterWithKey (\pos _ -> uncurry (-) pos == targetDiff) (posMap b)
      sortedCells = L.sortOn fst (M.toList cells)
      pad = replicate (2 * abs targetDiff) ["      "]
  in
   concat $ ["      "] : pad ++ ["______"] : map (uncurry printCellWithPos) sortedCells ++ pad

pad :: Int -> Char -> String -> String
pad n c
  | n <= 1 = id
  | otherwise = pad (n - 1) c . (:) c

printCellWithPos :: (Int, Int) -> Cell -> [String]
printCellWithPos (x, y) c =
  case c of
   Black -> ["      ", " \x1b[30m▗██▖\x1b[0m ", " \x1b[30m▝██▘\x1b[0m ", "______"]
   White -> ["      ", " \x1b[37;1m▗██▖\x1b[0m ", " \x1b[37;1m▝██▘\x1b[0m ", "______"]
   Empty -> ["      ", pad 6 ' ' $ show x, pad 6 ' ' $ show y, "______"]

makeCols :: Board -> [[String]]
makeCols b =
  let m = diam b * 2
  in
   map
   (\i -> if even i
         then makeSepCol b (i `div` 2)
         else makeCellCol b (i `div` 2))
   [0..m]
