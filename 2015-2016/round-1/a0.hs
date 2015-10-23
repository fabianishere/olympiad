import Control.Applicative

main :: IO ()
main =  show . sum . map read . words <$> getLine >>= putStrLn