import Control.Applicative
import Data.Char
main :: IO ()
main = do
    line <- getLine
    let names = map (map toUpper) $ filter (isUpper . head) $ words line
    putStrLn $ (surname $ last names) ++ (firstname $ head names)
    where
        surname xs = head xs : last xs : []
        firstname (x:_) = [x]