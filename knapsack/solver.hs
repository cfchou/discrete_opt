{-# LANGUAGE DeriveDataTypeable #-}
module KSSolver where
import System.Console.CmdArgs
import System.IO
import Control.Monad
import Control.Applicative
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Debug.Trace

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
                           tbl      = initTable k' vs ws
                       in  [tbl ! k' ! n']

initK :: [String] -> (Vector Int, Vector Int)
initK ss = let (vs, ws) = foldr (f . words) ([], []) ss
           in  (V.fromList vs, V.fromList ws)
    where f [] = id
          f (v:[]) = id
          f (v:w:_) = \(vs', ws') -> ((read v):vs', (read w):ws')
          
initTable :: Int -> Vector Int -> Vector Int -> Vector (Vector Int)
initTable k vs ws = V.generate (k + 1) gen
    where tbl = initTable k vs ws
          n = V.length vs
          gen w
            | w == 0 = V.replicate (n + 1) 0
            | otherwise = V.generate (n + 1) (gen' w)
          gen' _ 0 = 0
          gen' w' i = let w_i = ws ! (i - 1)
                          no_i = tbl ! w' ! (i - 1)
                          ok_i = tbl ! (w' - w_i) ! (i - 1) + (vs ! (i - 1))
                      in  if w' < w_i then no_i
                          else max no_i ok_i

{--
 V(w, i) = if (w < w_i) then V(w, i - 1)
           else max [V(w - w_i, i - 1),
                     V(w, i - 1)]
 --}

vw = ["4 11", "8 4", "10 5", "15 8", "4 3"]
(vs, ws) = initK vw
