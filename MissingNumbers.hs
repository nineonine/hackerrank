module MissingNUmbers where

import Control.Applicative ((<$>), (<*>))
import qualified Data.IntMap.Strict as M
import qualified Data.List as L

readNums = map (read :: String -> Int ) . words

countVals :: [Int] -> M.IntMap Int
countVals [] = M.empty
countVals xs = L.foldl' ( \acc x ->  case M.lookup x acc of Just n -> M.insert x (n+1) acc; _ -> M.insert x 1 acc ) M.empty xs

missingNumbers :: (Num a, Ord a) => M.IntMap a -> M.IntMap a -> [M.Key]
missingNumbers a b = L.map fst . M.toList . M.filter (> 0) $ M.unionWith (-) b a

main :: IO ()
main = do
    let process = fmap readNums (getLine >> getLine)
    (a,b) <- (,) <$> process <*> process
    putStrLn . unwords . map show $ missingNumbers (countVals a) (countVals b)
