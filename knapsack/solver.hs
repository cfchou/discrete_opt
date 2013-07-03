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
import Data.List (sort, sortBy)
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

-- format output
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

-- generating a 0-1 series of size n, if a index is in xs then 1 is given,
-- otherwise 0. 
-- xs are item indices in ascending order, 
to_bimap :: Int -> [Int] -> [Int]
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



--------
-- solution 6: Branch & Bound for the optimal and selected elements
ans7 :: Int -> Int -> Vector Int -> Vector Int -> [Int]
ans7 n k vs ws = 
    let (x:xs) = run_bnb n k vs ws
    in  x:(to_bimap n $ sort xs)

data Frac = Frac { fv :: Int
                 , fw :: Int
                 , delta :: Int
                 } deriving (Show)

-- cache calculated estimation
data Est = Est { big_v :: Int
               , big_w :: Int
               , fracAt :: Int
               , frac :: Frac
               } deriving (Show)

data Bound = Bound { esti :: Est
                     , accv :: Int
                     , accw :: Int
                     , curr :: Int
                     , clst :: [Int]
                     } deriving (Show)

-- sortBy v/w in decending order, also drop those with weights larger than 
-- capacity.
better_sorted :: Vector Int -> Vector Int -> Int -> Array Int (Int, Int, Int)
better_sorted vs ws k = 
    array (1, length zipped) $ zip [1..] zipped
    where zipped = dropWhile (\(_, w, _) -> w > k) $ sortBy (flip f) $ 
                    zip3 (V.toList vs) (V.toList ws) [1..]
          f (v1, w1, _) (v2, w2, _) = compare (v1 * w2) (v2 * w1) 
 
run_bnb :: Int -> Int -> Vector Int -> Vector Int -> [Int]
run_bnb n k vs ws = 
    let arr = better_sorted vs ws k
        (_, n') = A.bounds arr
        est = init_est arr k
        (lcb, lcl) = bnb3 arr n' k 1 True (Bound est 0 0 0 []) []
        (opt, lst) = bnb3 arr n' k 1 False (Bound est 0 0 lcb lcl) []
    in  trace (__tr n' est arr) $ opt : lst
    where __tr n est arr = ">>>>>>>>>>>>>>>>>\n" ++ show est ++ "\n" ++ 
                         show n ++ ", " ++ show k ++ 
                         "\n" ++ show arr


bnb3 :: Array Int (Int, Int, Int) -> Int -> Int -> Int -> Bool -> Bound -> [Int] 
    -> (Int, [Int])
bnb3 arr n k i t b xs = trace ("> (" ++ show i ++ ", " ++ show t ++ "), (" ++ 
                            show (accv b) ++ ", " ++ show (accw b) ++ ")\n" ++
                            show (esti b)) $ bnb3' arr n k i t b xs
bnb3' arr n k i t b xs
    | i > n || (t && wi > k - (accw b)) = trace ("-------------") $
        if (accv b) > cb then (accv b, xs)
        else (cb, cl)
    | otherwise =
        let est' = count_est arr (esti b) k i t
            (lcb, lcl) = bnb3 arr n k (i + 1) True (Bound est' accv' accw' cb cl) 
                            xs'
        -- in  if (est_val est') < cb then (cb, cl)
        in  if not (est_better est' cb) then (cb, cl)
            else bnb3 arr n k (i + 1) False (Bound est' accv' accw' lcb lcl) xs'

    where (vi, wi, idx) = arr A.! i
          cb = curr b
          cl = clst b
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
    else (fv frac') * (delta frac') > (cb - big_v') * (fw frac')
    where big_v' = big_v est
          frac' = frac est
    

count_est :: Array Int (Int, Int, Int) -> Est -> Int -> Int -> Bool -> Est
count_est _ est _ _ True = est
count_est arr est k i False =
    let (vi, wi, _) = arr A.! i 
        big_v' = (big_v est) - vi 
        big_w' = (big_w est) - wi 
    in  rest_est arr (k - big_w') $ est { big_v = big_v'
                                        , big_w = big_w' 
                                        , frac = zero_frac }
        

zero_frac = Frac 0 0 0

init_est :: Array Int (Int, Int, Int) -> Int -> Est
init_est arr k = rest_est arr k (Est 0 0 1 zero_frac)

rest_est :: Array Int (Int, Int, Int) -> Int -> Est -> Est
rest_est arr k e
    | (fracAt e) > n = 
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
                     fracAt = (fracAt e) + 1 }
        --in rest_est arr (k - wi) e'
        in rest_est arr (k - wi) $ trace ("  o " ++ show k ++ ", " ++ show wi 
                                    ++ " " ++ show e ++ "  -->  " ++ show e') e'
    where (vi, wi, _) = arr A.! (fracAt e)
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
