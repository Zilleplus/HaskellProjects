module Sud.Game(
    Game(..),
    solve
) where

import Control.Monad

type Game = [[Int]]

solve :: Game -> Game
solve g = g

-- solve algo
-- -> dynamic programming:
-- 1. find empty spot
-- 2. find all possible inputs for spot
-- 3. foreach possible input call solve
--     \-> if call returns success full, return solution
--     \-> if call fails -> try the next 
--     \-> if all tried and failed -> return failure

getEmptySpots :: Game -> [(Int, Int)]
getEmptySpots g = fmap snd  empty_spots
    where
        flat_game = join g
        indexed_game = zip flat_game $ [(i, j) | i <- [0..8] , j <- [0..8]]
        empty_spots = filter (\x -> fst x == 0) indexed_game

getCol :: Game -> Int -> [Int]
getCol g i = fmap (!! i) g

getRow :: Game -> Int -> [Int]
getRow g i = g !! i -- TODO: use ^? operator here

-- return the plane of a perticular position
-- args: (row_index, col_index)
getPlane :: Game -> (Int, Int) -> [Int]
getPlane g (row, col) = rows >>= filter_rows
     where 
        row_plane_index = floor $ toRational row / 3
        col_plane_index = floor $ toRational col / 3
        row_indices = [i + row_plane_index*3 | i <- [1 .. 3]]
        rows = fmap (getRow g) row_indices
        drop_before_plane = drop (col_plane_index*3) 
        drop_after_plane =  take 3
        filter_rows = drop_after_plane . drop_before_plane

hasHoles :: Game -> Bool
hasHoles g = or $ fmap (elem 0) g

hasConflict :: Game -> Int -> (Int, Int) -> Bool
hasConflict g num (row, col) = num `elem`  all
    where
        all = getPlane g (row, col) <>  getCol g col <> getRow g row

getFittingNumbers :: Game -> (Int, Int) -> [Int]
getFittingNumbers g (row, col) = []

testGame :: Game
testGame = [ [0,0,3,0,2,0,6,0,0]
            ,[9,0,0,3,0,5,0,0,1]
            ,[0,0,1,8,0,6,4,0,0]
            ,[0,0,8,1,0,2,9,0,0]
            ,[7,0,0,0,0,0,0,0,8]
            ,[0,0,6,7,0,8,2,0,0]
            ,[0,0,2,6,0,9,5,0,0]
            ,[8,0,0,2,0,3,0,0,9]
            ,[0,0,5,0,1,0,3,0,0]]
