module Matrix (transponse',
               addMatrices,
               getRow,
               getCol,
               multMatrices) where

import Data.Char
import Data.List

type Matrix = [[Double]]

-- Checks if a matrix is well formed.
-- Matrix is well formed if all of its rows are of equal lengths.
isWellFormed :: Matrix -> Bool
isWellFormed (x:xs) = and $ map ((== length x) . length) xs
isWellFormed _       = False

-- Gets dimensions of a n x m matrix as a pair (n, m).
size :: Matrix -> (Int,Int)
size []      = error "Matrix is malformed"
size m@(h:_) | isWellFormed m = (length m, length h)
             | otherwise = error "Matrix is malformed"

-- Gets matrix's element at given position.
getElement :: Matrix -> Int -> Int -> Double
getElement m i j | not $ isWellFormed m = error "Matrix is malformed"
                 | i < 0 || i >= h ||  j < 0 || j >= w =
                       error "Index out of bounds"
                 | otherwise = (m !! i) !! j
                 where (h, w) = size m

-- Returns the i-th row of a matrix.
getRow :: Matrix -> Int -> [Double]
getRow m i | not $ isWellFormed m = error "Matrix is malformed"
           | i < 0 || i >= h = error "Index out of bounds"
           | otherwise = m !! i
           where (h, _) = size m

-- Returns the j-th column of a matrix.
getCol :: Matrix -> Int -> [Double]
getCol m j | not $ isWellFormed m = error "Matrix is malformed"
           | j < 0 || j >= w = error "Index out of bounds"
           | otherwise = [r !! j | r <- m]
           where (_, w) = size m

-- Sums elements of lists with same indexes.
sumEqIndexes :: [Double] -> [Double] -> [Double]
sumEqIndexes x y = [a + b | (a, b) <- zip x y]

-- Returns the sum of two matrices.
addMatrices :: Matrix -> Matrix -> Matrix
addMatrices ma mb
    | (not $ isWellFormed ma)
        || (not $ isWellFormed mb) = error "Matrix is malformed"
    | size ma /= size mb = error "Matrices are not of equal size"
    | otherwise = [sumEqIndexes x y | (x, y) <- zip ma mb]

-- Returns a transposed version of the given matrix.
transponse' :: Matrix -> Matrix
transponse' m | isWellFormed m = [getCol m c | c <- [0..(snd $ size m) - 1]]
              | otherwise = error "Matrix is malformed"

-- Checks if two matrices can be multiplied.
-- In order for two matrices to be multipliable, number of columns of
-- first matrix needs to be equal to number of rows of second matrix.
multipliable :: Matrix -> Matrix -> Bool
multipliable ma mb = snd (size ma) == fst (size mb)

-- Multiplies two matrices.
multMatrices :: Matrix -> Matrix -> Matrix
multMatrices ma mb
    | not $ isWellFormed ma && isWellFormed mb =
        error "Matrix is malformed"
    | not $ multipliable ma mb =
        error "Incompatible matrix dimensions"
    | otherwise = [multSingleRow r mb | r <- ma]

-- Multiplies elements of lists with same indexes.
multEqIndexes :: [Double] -> [Double] -> [Double]
multEqIndexes = zipWith (*)

-- Multiplies a single matrix row with other matrix.
multSingleRow :: [Double] -> Matrix -> [Double]
multSingleRow r mb = [sum $ multEqIndexes r c | c <- transponse' mb]
