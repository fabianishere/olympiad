import Control.Applicative
main :: IO ()
main = do
    n <- read <$> getLine :: IO (Int)
    let range = [-n+1..n-1]
    putStrLn $ unlines [[if abs(x) <= abs(y) then '*' else '-' | x <- range] | y <- range]