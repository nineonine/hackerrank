-- Enter your code here. Read input from STDIN. Print output to STDOUT
import qualified Data.Char as Char

superdigit :: String -> String
superdigit n = case length n of
    1 -> n
    _ -> superdigit . show . sum $ map Char.digitToInt n

main :: IO ()
main = do
    [n, k] <- fmap words getLine
    putStrLn . superdigit . concat $ replicate (read k :: Int) n
