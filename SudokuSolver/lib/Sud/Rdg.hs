module Sud.Rdg(parseSudoku) where

import qualified Data.Text as T
import Data.Char

import Sud.Game as G

parseSudoku :: T.Text -> G.Game
parseSudoku text = fmap parseLine ls
    where
        parseLine = T.foldl (\acc elem -> mappend acc [digitToInt elem]) []
        ls = T.lines text
