module Lib (
    parse,
    solve,
    Cell (..),
) where

import Data.List (transpose)

type Grid a = [Row a]
type Row a = [a]

data Cell = Empty | Queen | Blocked deriving (Eq)

instance Show Cell where
    show Empty = "_"
    show Queen = "Q"
    show Blocked = "_"

parse :: String -> Grid Cell
parse = map (map parseCell) . lines
  where
    parseCell '.' = Empty
    parseCell 'Q' = Queen
    parseCell _ = error "parse: invalid cell"

rows :: Grid a -> [Row a]
rows = id

cols :: Grid a -> [Row a]
cols = transpose

diags :: Grid a -> [Row a]
diags grid = [extractDiagonal i | i <- [-(n - 1) .. n - 1]]
  where
    n = length grid -- Assuming a square matrix
    extractDiagonal k = [grid !! (j + k) !! j | j <- [0 .. n - 1], j + k >= 0, j + k < n]

diagsInv :: [Row a] -> Grid a
diagsInv ds = go [] ds
  where
    dim = (length ds + 1) `div` 2
    go :: [Row a] -> [Row a] -> Grid a
    go acc [] = reverse acc
    go acc ds =
        let (used, unused) = splitAt dim ds -- Separate the first 'dim' diagonals and the rest.
            newRow = reverse $ map head used -- Extract the first element from each used diagonal.
            newDiags = [tail d | d <- used, not (null (tail d))] ++ unused -- Drop the first element from used diagonals if not empty.
         in go (newRow : acc) newDiags

antiDiags :: Grid a -> [Row a]
antiDiags = map reverse . diags . map reverse

antiDiagsInv :: [Row a] -> Grid a
antiDiagsInv = map reverse . diagsInv . map reverse

-- | Prune a grid by removing all cells that are in the same row, column or diagonal as a queen.
prune :: Grid Cell -> Grid Cell
prune =
    pruneBy antiDiags antiDiagsInv
        . pruneBy diags diagsInv
        . pruneBy cols cols
        . pruneBy rows rows
  where
    pruneBy f fI = fI . map applyPrune . f
    applyPrune group =
        if Queen `elem` group
            then map (\cell -> if cell == Queen then Queen else Blocked) group
            else group

-- create a list of grids that contain a new queen in each of the empty cells of the first row with an empty cell
collapse :: Grid Cell -> [Grid Cell]
collapse grid = map (prune . (\r -> rsB ++ r : rsA)) (go [] row)
  where
    (rsB, row : rsA) = break (elem Empty) grid
    go acc xs = if all (== Blocked) xs then acc else go (x' : acc) (xB ++ Blocked : xA)
      where
        (xB, _ : xA) = break (== Empty) xs
        x' = xB ++ Queen : xA

unsolvable :: Grid Cell -> Bool
unsolvable grid = numBlocked > nSqr - n
  where
    numBlocked = length $ concatMap (filter (== Blocked)) grid
    n = length grid
    nSqr = n * n

solved :: Grid Cell -> Bool
solved grid = numQueens == n
  where
    numQueens = length $ concatMap (filter (== Queen)) grid
    n = length grid

search :: Grid Cell -> [Grid Cell]
search grid
    | unsolvable grid = []
    | solved grid = [grid]
    | otherwise =
        concatMap
            (search . prune)
            $ collapse grid

solve :: Grid Cell -> [Grid Cell]
solve = search . prune
