{-# LANGUAGE DeriveDataTypeable #-}
module KSSolver where
import System.Console.CmdArgs
import System.IO
import Control.Monad
import Control.Applicative
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Debug.Trace
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.DeepSeq (($!!))

data Input = Input { file :: FilePath }
             deriving (Show, Data, Typeable)

input = Input { file = def &= help "Data file name" &= typ "FilePath" }

main = run . lines <$> (cmdArgs input >>= readFile . file) >>= 
       print

run :: [String] -> [Int]
run ss
    | length ss == 0 = [0]
    | otherwise = 
        case words (head ss) of
            []      -> [0]
            n:[]    -> [0]
            n:k:_   -> let k' = read k
                           n' = read n
                           (vs, ws) = initK (tail ss)
                       in  ans n' k' vs ws

ans2:: Int -> Int -> Vector Int -> Vector Int -> [Int]
ans2 n k vs ws = 
    let tbl = initTable k vs ws tbl
    in  [tbl ! k ! n ]


initK :: [String] -> (Vector Int, Vector Int)
initK ss = let (vs, ws) = foldr (f . words) ([], []) ss
           in  (V.fromList vs, V.fromList ws)
    where f [] = id
          f (v:[]) = id
          f (v:w:_) = \(vs', ws') -> ((read v):vs', (read w):ws')
          
initTable :: Int -> Vector Int -> Vector Int -> Vector (Vector Int) -> 
    Vector (Vector Int)
initTable k vs ws tbl = V.generate (k + 1) gen
    where n = V.length vs
          gen w
            | w == 0 = V.replicate (n + 1) 0
            | otherwise = V.generate (n + 1) (gen' w)
          gen' _ 0 = 0
          gen' w' i = let w_i = ws ! (i - 1)
                          no_i = tbl ! w' ! (i - 1)
                          ok_i = tbl ! (w' - w_i) ! (i - 1) + (vs ! (i - 1))
                      in  if w' < w_i then no_i
                          else max no_i ok_i

ans:: Int -> Int -> Vector Int -> Vector Int -> [Int]
ans n k vs ws = 
    let row = initRow k vs ws
    in  [row ! k]

initRow :: Int -> Vector Int -> Vector Int -> Vector Int
initRow k vs ws = itbl 1 $!! V.replicate (k + 1) 0
    where n = V.length vs
          itbl i row
             | i > n = row
             | otherwise = itbl (i + 1) $!! V.generate (k + 1) gen
             {--
             | otherwise = 
                 let row' = V.generate (k + 1) gen
                 in  itbl (i + 1) $ trace (show row') row'
             --}
             where gen w = 
                       let w_i = ws ! (i - 1)
                           no_i = row ! w
                           ok_i = row ! (w - w_i) + (vs ! (i - 1))
                       in  
                           if w < w_i then no_i
                           else max no_i ok_i



-------------------------------------
-- Tests
-------------------------------------


initTable2 k vs ws tbl = V.generate (k + 1) gen
    where n = V.length vs
          gen w
            | w == 0 = V.replicate (n + 1) 0
            | otherwise = V.generate (n + 1) (gen' w)
          gen' w' i = let v = tbl ! (w' - 1) ! i
                      in  v + 1
                          
          
initTable3 :: Int -> Int -> Vector (Vector Int) -> Vector (Vector Int)
initTable3 k n tbl = V.generate (k + 1) gen
    where gen w
            | w == 0 = V.replicate (n + 1) 0
            | otherwise = 
                if (w `mod` 10 == 0) then
                    V.generate (n + 1) (gen' (trace (show w) w))
                else
                    V.generate (n + 1) (gen' w)
          gen' w' i = let v = tbl ! (w' - 1) ! i
                          --v' = trace (show v) v
                      in  v + 1


test k n = let tbl = initTable3 k n tbl
           in  tbl ! k ! n

{-- 
 Fomula:
 V(w, i) = if (w < w_i) then V(w, i - 1)
           else max [V(w - w_i, i - 1) + v_i,
                     V(w, i - 1)]
 --}

k = 11
n = 4
vw = ["8 4", "10 5", "15 8", "4 3"]
(vs, ws) = initK vw
