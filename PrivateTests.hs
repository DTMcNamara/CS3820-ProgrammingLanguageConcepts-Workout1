module PrivateTests where

import Control.Applicative
import Basic

-- test x y n means to run test number n, to check that x equals y.
test :: (Show a , Ord a) => a -> a -> Int -> IO ()
test x y n =
  let name = "test for p" ++ show n in
  if x == y then
    putStrLn ("Passing " ++ name ++ ".")
  else
    putStrLn ("Failing " ++ name ++ ": expected " ++ show y ++ ", computed " ++ show x)

main :: IO ()
main =
  do
    {- zipWith id fs xs has the effect of applying a list of functions fs to the corresponding inputs in xs.
       So (zipWith id tests [1..]) has type [IO ()], because each element in tests has type Int -> IO ().
       So we compute which test is which very easily, without having to write down the test number as
       part of the calls to test below.

       This value of type [IO ()] can then be evaluated in order using sequence_,
       from Control.Applicative. -}
    putStrLn ("Executing " ++ show (length tests) ++ " tests.")
    sequence_ (zipWith id tests [1..])
  where
    -- it is nice we can just write a list of tests compactly as follows:
    tests = 
      [

        test (p1 (-1,0,3333333)) 3333332 ,
        test (p2 (False,2/3)) (2/3,False) ,
        test (p3 (1,-10) (2,4)) (3,-6) ,
        test (p4 ('a','b') (False,True)) ('a','b',False,True) ,
        test (p5 (id) (10,20)) (10,20) ,
        
        test (p6 "c n") "c nc n",
        test (p7 "n1b") "b1n",
        test (p8 "the weather is" "frightful") "the weather is is frightful",
        test (p9 (1 / 1)) "1 % 1",
        test (p10 (1 / 1)) "1 / 1",
        
        test (p11 (*) 100) 10000,
        test (p12 (==) abs (-5)) False,
        test (p13 (2,3) abs (uncurry (+))) 5,
        
        test (p14 (Nothing) 0 (+ 10)) 0,
        test (p15 (Just 5) (Just 3)) (Just 5),

        test (p16 [0,0,0,2,3]) 2,
        test (p17 [1,2] [3,4,5,5] [6,7,8,9]) [1,2,3,4,5,5,6,7,8,9],
        test (p18 [0,1,2,3,4,5]) [1,2,3,4],
        test (p19 (* 3) even [0,1,2,3,4,5,6]) [0,2,4,6],
        test (p20 (+ 1) [1,2,3,4]) [(1,2),(2,3),(3,4),(4,5)]
      ]
      
