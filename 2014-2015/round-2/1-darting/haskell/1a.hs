import System.IO
import Control.Monad

main = do
	n <- readLn :: IO Int
	input <- replicateM n (readLn :: IO Int)
	print (n : input)