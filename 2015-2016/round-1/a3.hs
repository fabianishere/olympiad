import Control.Applicative
import Data.List
main :: IO ()
main = do
        l <- lines <$> getContents
        putStrLn $ show $ length $ filter zevenachtig $ takeWhile (/= 0) (read <$> l :: [Int])
        where zevenachtig x = x `mod` 7 == 0 || isInfixOf "7" (show x)