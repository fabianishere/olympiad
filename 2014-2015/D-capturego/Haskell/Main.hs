--
-- Copyright 2014 Fabian M.
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--	http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
import Data.Maybe
import Data.Either
import Data.List
import Data.Char

-- Main entry point
main :: IO ()
main = do
	start <- getLine
	case start of
		"Start " -> putStrLn "Starting"
		_ -> putStrLn $ marshall (unmarshall start)



-- unmarshall the given string containing the move
unmarshall :: String -> Int
unmarshall s = ord (toUpper (head s)) - 65 + (read [(s !! 1)] - 1) * 10

-- marshall the given move
marshall :: Int -> String
marshall k =  [chr (65 + k `mod` 10), chr (k `quot` 10 + 49)]

data Stone = Black | White  deriving (Eq,Show)
type Intersection = Either Int Stone
type Board = [[Intersection]]
data GameState = Game { board :: Board, turn :: Stone }

-- create the initial game state
initialGameState :: GameState
initialGameState = Game (map (map Left) (map (\x -> map (\y -> 9 * (x - 1) + y) [1..9]) [1..9])) Black

-- list the possible moves to play
possibleMoves :: Board -> [Int]
possibleMoves board = [k | Left k <- concat board]

-- play a stone at a square
place :: Int -> GameState -> Maybe GameState
place k (Game board turn)
    | not (k `elem` possibleMoves board) = Nothing   -- illegal move
    | otherwise = Just $ Game (map (map replace) board) (switch turn)
    where
    replace (Left k') | k' == k = Right turn
    replace x                   = x

    switch Black = White
    switch White = Black

-- print the board
showSquare = either (\n -> " " ++ show n ++ " ") (concat . replicate 1 . show)

showBoard :: Board -> String
showBoard board =
      unlines . surround "+---+---+---+---+---+---+---+---+---+---+---+"
    . map (concat . surround "|". map showSquare)
    $ board
    where
    surround x xs = [x] ++ intersperse x xs ++ [x]

printBoard = putStr . showBoard
