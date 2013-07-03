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
import Data.Array (Array, array)
import qualified Data.Array as A

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
ansDP :: Int -> Int -> Vector Int -> Vector Int -> [Int]
ansDP n k vs ws = 
    let rt = (initRow3 k vs ws) ! k
    -- in  fst rt : (reverse (snd rt))
    in  fst rt : (to_bimap n $ reverse (snd rt))

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

-- better_sorted doesn't suffer from the precision problem in floats
better_sorted :: Vector Int -> Vector Int -> [Int]
better_sorted vs ws = 
    let (_, _, xs) = unzip3 $ sortBy (flip f) $ 
                        zip3 (V.toList vs) (V.toList ws) [1..]
    in  xs
    where f (v1, w1, _) (v2, w2, _) = compare (v1 * w2) (v2 * w1) 



run_bnb :: Int -> Int -> Vector Int -> Vector Int -> Int
run_bnb n k vs ws = 
    let rlst = better_sorted vs ws
        lcb = bnb vs ws n 1 True (Bound 0 k rlst 0)
    in  bnb vs ws n 1 False (Bound 0 k rlst lcb)

bnb :: Vector Int -> Vector Int -> Int -> Int -> Bool -> Bound -> Int
bnb vs ws n i t b 
    -- | i > n = acc b
    | i > n = max (acc b) cb
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
                    part = floor $ (fromIntegral ((vx * c) + (wx - 1))) / 
                                      (fromIntegral wx)


-- solution 5: Branch & Bound for the optimal and selected elements
ansBB :: Int -> Int -> Vector Int -> Vector Int -> [Int]
ansBB n k vs ws = 
    let (cb, cl) = run_bnb2 n k vs ws
    -- in  cb : cl
    in  cb : (to_bimap n cl) 

data Bound2 = Bound2 { bound :: Bound
                     , clst :: [Int]
                     } deriving (Show)

run_bnb2 :: Int -> Int -> Vector Int -> Vector Int -> (Int, [Int])
run_bnb2 n k vs ws = 
    let rlst = better_sorted vs ws
        (lcb, lcl) = bnb2 vs ws n 1 True (Bound2 (Bound 0 k rlst 0) []) []
        (rcb, rcl) = bnb2 vs ws n 1 False (Bound2 (Bound 0 k rlst lcb) lcl) []
    in  (rcb, reverse rcl)

bnb2 :: Vector Int -> Vector Int -> Int -> Int -> Bool -> Bound2 -> [Int] 
    -> (Int, [Int])
bnb2 vs ws n i t b2 xs
    -- | i > n = (acc b, xs)
    | i > n = if (acc b > cb) then (acc b, xs)
              else (cb, cl)
    | otherwise = --trace (show i ++ "  " ++ (show $ acc b) ++ ", " ++ (show $ curr b)) $
        if (t && wi > cap b) then (cb, cl) -- exceeds bound, pruned
        else 
            let acc' = (acc b) + (if t then vi else 0)
                cap' = (cap b) - (if t then wi else 0)
                xs' = if t then i:xs else xs
                (e, c, nlst) = estimate vs ws i acc' cap' (rank b) -- e >= acc'
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

--------
-- solution 6: Branch & Bound for the optimal and selected elements
ans7 :: Int -> Int -> Vector Int -> Vector Int -> [Int]
ans7 n k vs ws = [run_bnb3 n k vs ws] 

data Frac = Frac { v :: Int
                 , w :: Int
                 , delta :: Int
                 } deriving (Show)

data Est = Est { big_v :: Int
               , big_w :: Int
               , smallAt :: Int
               , frac :: Frac
               } deriving (Show)

data Bound3 = Bound3 { esti :: Est
                     , accv :: Int
                     , accw :: Int
                     , cap_ :: Int
                     , curr_ :: Int
                     , clst_ :: [Int]
                     } deriving (Show)

{--
better_sorted3 :: Vector Int -> Vector Int -> Array Int (Int, Int, Int)
better_sorted3 vs ws = 
    array (1, V.length vs) $ zip [1..] zipped
    where zipped = sortBy (flip f) $ zip3 (V.toList vs) (V.toList ws) [1..]
          f (v1, w1, _) (v2, w2, _) = compare (v1 * w2) (v2 * w1) 
--}

better_sorted3 :: Vector Int -> Vector Int -> Int -> Array Int (Int, Int, Int)
better_sorted3 vs ws k = 
    array (1, length zipped) $ zip [1..] zipped
    where zipped = dropWhile (\(_, w, _) -> w > k) $ sortBy (flip f) $ zip3 (V.toList vs) (V.toList ws) [1..]
          f (v1, w1, _) (v2, w2, _) = compare (v1 * w2) (v2 * w1) 
 
run_bnb3 :: Int -> Int -> Vector Int -> Vector Int -> Int
run_bnb3 n k vs ws = 
    let arr = better_sorted3 vs ws k
        (_, n') = A.bounds arr
        est = init_est arr k
        (lcb, lcl) = bnb3 arr n' 1 True (Bound3 est 0 0 k 0 []) []
    in  trace (__tr n' est arr) $ fst $ bnb3 arr n' 1 False (Bound3 est 0 0 k lcb lcl) []
    -- in  fst $ bnb3 arr n 1 False (Bound3 est 0 k lcb lcl) []
    where __tr n est arr = ">>>>>>>>>>>>>>>>>\n" ++ show est ++ "\n" ++ 
                         show n ++ ", " ++ show k ++ 
                         "\n" ++ show arr


bnb3 :: Array Int (Int, Int, Int) -> Int -> Int -> Bool -> Bound3 -> [Int] 
    -> (Int, [Int])
bnb3 arr n i t b xs = trace ("> (" ++ show i ++ ", " ++ show t ++ "), (" ++ 
                            show (accv b) ++ ", " ++ show (accw b) ++ ")\n" ++
                            show (esti b)) $ bnb3' arr n i t b xs
bnb3' arr n i t b xs
    | i > n || (t && wi > k - (accw b)) = trace ("-------------") $
        if (accv b) > cb then (accv b, xs)
        else (cb, cl)
    | otherwise =
        let est' = count_est arr (esti b) k i t
            (lcb, lcl) = bnb3 arr n (i + 1) True (Bound3 est' accv' accw' k cb cl) 
                            xs'
        -- in  if (est_val est') < cb then (cb, cl)
        in  if not (est_better est' cb) then (cb, cl)
            else bnb3 arr n (i + 1) False (Bound3 est' accv' accw' k lcb lcl) xs'

    where (vi, wi, idx) = arr A.! i
          cb = curr_ b
          cl = clst_ b
          k = cap_ b
          accv' = (accv b) + (if t then vi else 0)
          accw' = (accw b) + (if t then wi else 0)
          xs' = if t then idx:xs else xs
          -- __tr e = trace ((show i) ++ " " ++ (show t) ++ " " ++ (show e)) e

est_better :: Est -> Int -> Bool
est_better est cb = 
    let b = est_better' est cb
    in  trace ("    " ++ show b ++ "   [[ " ++ show est ++ " >>> " ++ show cb ++ " ]]\n") b
est_better' est cb =
    if big_v' > cb then True
    else (v frac') * (delta frac') > (cb - big_v') * (w frac')
    where big_v' = big_v est
          frac' = frac est
    


count_est :: Array Int (Int, Int, Int) -> Est -> Int -> Int -> Bool -> Est
count_est _ est _ _ True = est
count_est arr est k i False = -- rest_est arr k est
    let (vi, wi, _) = arr A.! i 
        big_v' = (big_v est) - vi 
        big_w' = (big_w est) - wi 
    --in  rest_est arr k (Est big_v' big_w' (smallAt est) 0)
    -- in  rest_est arr (k - (big_w est)) $ est { big_v = big_v'
    in  rest_est arr (k - big_w') $ est { big_v = big_v'
                                       , big_w = big_w' 
                                       , frac = zero_frac }
        

zero_frac = Frac 0 0 0

init_est :: Array Int (Int, Int, Int) -> Int -> Est
init_est arr k = rest_est arr k (Est 0 0 1 zero_frac)

rest_est :: Array Int (Int, Int, Int) -> Int -> Est -> Est
rest_est arr k e
    | (smallAt e) > n = 
        -- let e' = e { frac = zero_frac }
        trace ("  | " ++ show k 
                ++ " " ++ show e) e
    -- | k <= wi = e { small_v = floor $ (fromIntegral (vi * k + (wi - 1))) /
    --                            (fromIntegral wi) }
    | k < wi = 
        let e' = e { frac = Frac vi wi k }
        in  trace ("  x " ++ show k ++ ", " ++ show wi 
                ++ " " ++ show e ++ "  -->  " ++ show e') e'
    | otherwise = -- k >= wi
        let e' = e { big_v = (big_v e) + vi,
                     big_w = (big_w e) + wi,
                     smallAt = (smallAt e) + 1 }
        --in rest_est arr (k - wi) e'
        in rest_est arr (k - wi) $ trace ("  o " ++ show k ++ ", " ++ show wi 
                                    ++ " " ++ show e ++ "  -->  " ++ show e') e'
    where (vi, wi, _) = arr A.! (smallAt e)
          n = snd $ A.bounds arr
    




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
