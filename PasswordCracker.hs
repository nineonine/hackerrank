module PasswordCracker where

import qualified Data.List as L
import Control.Monad (replicateM)
import Data.Foldable (forM_)

type LoginAttempt = String
type Password     = String

getInputs :: IO ([Password], LoginAttempt)
getInputs = (,) <$> (getLine >> fmap words getLine) <*> getLine

mkOutput :: ([Password], LoginAttempt) -> String
mkOutput (ps, l) = L.unwords $ go ps ps l
                 where
                 go [] _ _ = ["WRONG"]
                 go _ _ [] = []
                 go (p:ps) pss l' | p `L.isPrefixOf` l' = p : go pss pss (drop (length p) l' )
                                  | otherwise           = go ps pss l'

solve :: ([Password], LoginAttempt) -> String
solve v = case last . words $ mkOutput v of
    "WRONG" -> "WRONG PASSWORD"
    _          -> mkOutput v

main :: IO ()
main = do
    numOfCases <- fmap read getLine :: IO Int
    cases <- replicateM numOfCases getInputs
    forM_ cases $ \c -> putStrLn $ solve c
