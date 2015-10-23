import Control.Applicative
main :: IO ()
main = do
    xs <- fmap read <$> lines <$> getContents :: IO [Int]
    results <- handleChunk ([], take (head xs) $ tail xs)
    putStrLn $ unlines $ show <$> results

handleChunk :: ([Int], [Int]) -> IO ([Int])
handleChunk ([], []) = return []
handleChunk (xs, []) = return [maximum xs]
handleChunk ([], ys) = handleChunk $ splitAt 10 ys
handleChunk (xs, ys) = (max' :) <$> handleChunk (splitAt 10 $ tail $ dropWhile (/= max') xs ++ ys)
    where max' = maximum xs
