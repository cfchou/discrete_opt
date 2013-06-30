{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Data.Vector.Unboxed as UV
import qualified Data.List as L
import Data.List (sortBy)
import Data.Function (on)

{-- 
 0-1 Knapsack Formula:
 V(w, i) = if (w < w_i) then V(w, i - 1)
           else max [V(w - w_i, i - 1) + v_i,
                     V(w, i - 1)]
 --}

data Input = Input { file :: FilePath }
             deriving (Show, Data, Typeable)

input = Input { file = def &= help "Data file name" &= typ "FilePath" }

main = run . lines <$> (cmdArgs input >>= readFile . file) >>= 
       print2


print2 :: [Int] -> IO () 
print2 xs = 
    putStrLn (show (head xs) ++ " 1") >>
    (putStrLn $ L.concat $ L.intersperse " " [show x | x <- tail xs])



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
                       in  ans7 n' k' vs ws

initK :: [String] -> (Vector Int, Vector Int)
initK ss = let (vs, ws) = foldr (f . words) ([], []) ss
           in  (V.fromList vs, V.fromList ws)
    where f [] = id
          f (v:[]) = id
          f (v:w:_) = \(vs', ws') -> ((read v):vs', (read w):ws')
          

-- select approach, either "Dynamic Programming" or "Branch and Bound".
-- let r = v/w, if all r's are about the same, choose DP.
ans4:: Int -> Int -> Vector Int -> Vector Int -> [Int]
ans4 _ _ vs ws = 
    --let r = choose (pi / 9) vs ws
    --in  if (r == "DP") then [0] else [1]
    choose (pi / 18) vs ws

{--
choose :: Double -> Vector Int -> Vector Int -> String
choose d vs ws = if (V.any f vw) then "BB"
                 else "DP"
--}
choose :: Double -> Vector Int -> Vector Int -> [Int]
choose d vs ws = 
    let (pa, pb) = V.partition id $ V.map f vw
    in  [V.length pa, V.length pb]
    where n  = fromIntegral $ V.length vs :: Double
          ev = (fromIntegral (V.sum vs)) / n
          ew = (fromIntegral (V.sum ws)) / n
          e = ev^2 + ew^2
          vw = V.zipWith (\v -> \w -> (fromIntegral v, fromIntegral w)) vs ws
          f (v, w) = ((v * ev + w * ew)^2) < ((v^2 + w^2) * e) * (cos d)
          -- "< (cos d)" means more widespread, v/w more differs from each 
          -- other, BB is more likely to help.




-- solution 1: 2D table
ans2 :: Int -> Int -> Vector Int -> Vector Int -> [Int]
ans2 n k vs ws = 
    let tbl = initTable k vs ws tbl
    in  [tbl ! k ! n ]


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

-- solution 2: single row table (using DeepSeq)
ans3 :: Int -> Int -> Vector Int -> Vector Int -> [Int]
ans3 n k vs ws = 
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

-- solution 2: single row table (using DeepSeq + Unboxed Vector)
ans :: Int -> Int -> Vector Int -> Vector Int -> [Int]
ans n k vs ws = 
    let row = initRow2 k vs ws
    in  [row UV.! k]

initRow2 :: Int -> Vector Int -> Vector Int -> UV.Vector Int
initRow2 k vs ws = itbl 1 $!! UV.replicate (k + 1) 0
    where n = V.length vs
          itbl i row
             | i > n = row
             | otherwise = itbl (i + 1) $!! UV.generate (k + 1) gen
             where gen w = 
                       let w_i = ws ! (i - 1)
                           no_i = row UV.! w
                           ok_i = row UV.! (w - w_i) + (vs ! (i - 1))
                       in  
                           if w < w_i then no_i
                           else max no_i ok_i


to_bimap n xs = reverse . snd $ foldl f (xs, []) [1..n]
    where f ([], es) i = ([], 0:es)
          f (a@(x:xs), es) i = if (i == x) then (xs, 1:es)
                             else (a, 0:es)

-- solution 3: 
ans5 :: Int -> Int -> Vector Int -> Vector Int -> [Int]
ans5 n k vs ws = 
    let rt = (initRow3 k vs ws) ! k
    -- in  fst rt : (reverse (snd rt))
    in  fst rt : (to_bimap n $ reverse (snd rt))
{--
ans5 n k vs ws = 
    let rt = (initRow3 k vs ws) ! k
        ps = reverse . snd $ foldl f (reverse $ snd rt, []) [1..n]
    in  fst rt : ps
    where f ([], es) i = ([], 0:es)
          f (a@(x:xs), es) i = if (i == x) then (xs, 1:es)
                             else (a, 0:es)
--}

initRow3 :: Int -> Vector Int -> Vector Int -> Vector (Int, [Int])
initRow3 k vs ws = itbl 1 $!! (V.replicate (k + 1) (0, []))
    where n = V.length vs
          itbl i row
             | i > n = row
             | otherwise = itbl (i + 1) $!! V.generate (k + 1) gen
             where gen w = 
                       let w_i = ws ! (i - 1)
                           w_r = w - w_i
                           no_i = fst $ row ! w
                           ok_i = (fst $ row ! w_r) + (vs ! (i - 1))
                       in  
                           if w < w_i then (no_i, snd (row ! w))
                           else if no_i > ok_i then (no_i, snd (row ! w))
                                else (ok_i, i : snd (row ! w_r))



-- solution 4: Branch & Bound for the optimal
ans6 :: Int -> Int -> Vector Int -> Vector Int -> [Int]
ans6 n k vs ws = [run_bnb n k vs ws]

data Bound = Bound { acc :: Int
                   , cap :: Int
                   , rank :: [Int]
                   , curr :: Int
                   } deriving (Show)

ratios :: Vector Int -> Vector Int -> Vector Double
ratios = V.zipWith (\a -> \b -> (fromIntegral a) / (fromIntegral b))

sorted :: Vector Int -> Vector Int -> [Int]
sorted vs ws = snd . unzip $ sortBy (flip (compare `on` fst)) 
                   $ zip (V.toList $ ratios vs ws) [1..] 

run_bnb :: Int -> Int -> Vector Int -> Vector Int -> Int
run_bnb n k vs ws = 
    let rlst = sorted vs ws
        lcb = bnb vs ws n 1 True (Bound 0 k rlst 0)
    in  bnb vs ws n 1 False (Bound 0 k rlst lcb)

bnb :: Vector Int -> Vector Int -> Int -> Int -> Bool -> Bound -> Int
bnb vs ws n i t b 
    | i > n = acc b
    | otherwise = --trace (show i ++ "  " ++ (show $ acc b) ++ ", " ++ (show $ curr b)) $
        -- if (t && wi > cap b) then max (acc b) cb -- exceeds bound, pruned
        if (t && wi > cap b) then cb -- exceeds bound, pruned
        else 
            let acc' = (acc b) + (if t then vi else 0)
                cap' = (cap b) - (if t then wi else 0)
                (e, c, nlst) = estimate vs ws i acc' cap' (rank b)
                lcb = bnb vs ws n (i + 1) True (Bound acc' cap' nlst cb)
            in  if e <= cb then cb -- estimation isn't better, pruned
                else bnb vs ws n (i + 1) False (Bound acc' cap' nlst lcb)
    where wi = ws ! (i - 1)
          vi = vs ! (i - 1)
          cb = curr b

estimate :: Vector Int -> Vector Int -> Int -> Int -> Int -> [Int] 
    -> (Int, Int, [Int])
estimate vs ws i acc cap rlst = 
    (\(a, b, c) -> (a, b, reverse c)) $ f acc cap [] rlst
    where f est c newlst [] = (est, c, newlst)
          f est c newlst xss@(x:xs) = 
              if x == i then 
                  f est c newlst xs
              else 
                  if wx > c then (est + part, 0, 
                                  (filter (/= i) (reverse xss)) ++ newlst)
                  else f (est + vx) (c - wx) (x:newlst) xs
              where wx = ws ! (x - 1)
                    vx = vs ! (x - 1)
                    -- ceiling, optimistic on estimation
                    part = ceiling $ (fromIntegral (vx * c)) / 
                                      (fromIntegral wx)


-- solution 5: Branch & Bound for the optimal and selected elements
ans7 :: Int -> Int -> Vector Int -> Vector Int -> [Int]
ans7 n k vs ws = 
    let (cb, cl) = run_bnb2 n k vs ws
    -- in  cb : cl
    in  cb : (to_bimap n cl) 

data Bound2 = Bound2 { bound :: Bound
                     , clst :: [Int]
                     } deriving (Show)

run_bnb2 :: Int -> Int -> Vector Int -> Vector Int -> (Int, [Int])
run_bnb2 n k vs ws = 
    let rlst = sorted vs ws
        (lcb, lcl) = bnb2 vs ws n 1 True (Bound2 (Bound 0 k rlst 0) []) []
        (rcb, rcl) = bnb2 vs ws n 1 False (Bound2 (Bound 0 k rlst lcb) lcl) []
    in  (rcb, reverse rcl)

bnb2 :: Vector Int -> Vector Int -> Int -> Int -> Bool -> Bound2 -> [Int] 
    -> (Int, [Int])
bnb2 vs ws n i t b2 xs
    | i > n = (acc b, xs)
    | otherwise = --trace (show i ++ "  " ++ (show $ acc b) ++ ", " ++ (show $ curr b)) $
        if (t && wi > cap b) then (cb, cl) -- exceeds bound, pruned
        else 
            let acc' = (acc b) + (if t then vi else 0)
                cap' = (cap b) - (if t then wi else 0)
                xs' = if t then i:xs else xs
                (e, c, nlst) = estimate vs ws i acc' cap' (rank b)
                (lcb, lcl) = bnb2 vs ws n (i + 1) True 
                                (Bound2 (Bound acc' cap' nlst cb) cl) xs'
            in  if e <= cb then (cb, cl) -- estimation isn't better, pruned
                else bnb2 vs ws n (i + 1) False 
                        (Bound2 (Bound acc' cap' nlst lcb) lcl) xs'
    where wi = ws ! (i - 1)
          vi = vs ! (i - 1)
          b = bound b2
          cb = curr b
          cl = clst b2

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

k = 11
n = 4
vw = ["8 4", "10 5", "15 8", "4 3"]
(vs, ws) = initK vw
