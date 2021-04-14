main :: IO ()
main = do printseq

-- Note that if the following was indented before printseq, GHC --
-- would treat the following lines as statements in do block    --
-- instead of as continuation of printseq.                      --

            -- Multiple ways to compose a function:             --
            [ (g . f) exampleList
            , g . f $ exampleList
            , g . (\lst -> (length lst, sum lst)) $ exampleList

            -- We can use ((g .) . f) to first provide two      --
            -- arguments to f apparently, in Data.Composition   --
            -- we can use .: for this.                          --
            , ((gf .) . h) 4 exampleList
            , (gf . (h 4)) exampleList
            , (gf . (\(x,y) -> h x y)) (4, exampleList) ]

            -- Parameter shadowing with let clause:             --
          printseq
            [ f1 2 7
            , f1 2 3 ]

            -- Parameter shadowing with where clause:           --
          printseq
            [ f2 2 7
            , f2 2 3 ]

          error "We can abort the program with error :: String -> a"

    where printseq :: Show n => [n] -> IO ()
        -- Functions can be declared in "where" block.          --
          printseq [] = return ()
          printseq (x:xs) = do
              putStrLn $ show $ x
              printseq xs
              return ()

          gf :: Fractional n => [n] -> n
          gf = g . f
          exampleList = [4, 12, 6]

-- Note that we can avoid offside rule (i.e. indentation rules) --
-- by placing expressions in curly braces and separating them   --
-- with semicolons; so, the printseq function above could be    --
-- written equivalently as:                                     --
--                                                              --
-- printseq (x:xs) = do {                                       --
--      putStrLn $ show $ x;                                    --
--  printseq xs;                                                --
--          return () }                                         --

f :: Fractional n => [n] -> (Int, n)
-- To do this the correct way, use Maybe (Int, n) instead.      --
f list = fstep list begin
    
    where fstep :: Fractional n => [n] -> (Int, n) -> (Int, n)
          fstep [] res = res
          fstep (x:xs) (cnt, sum') = fstep xs (cnt + 1, sum' + x)

          begin :: Fractional n => (Int, n)
          begin = (0, 0)

-- If we used name sum instead of sum', we would shadow the sum --
-- function from Prelude and get the shadowing warning.         --
g :: Fractional n => (Int, n) -> n
g (cnt, sum') = sum' / (fromIntegral cnt)

h :: Fractional n => Int -> [n] -> [n]
h a (x:xs) = (fromIntegral a * x):xs

-- Function parameters can be shadowed by let or where block.   --
-- To detect shadowing use -Wname-shadowing switch in GHC.      --

f1 :: Int -> Int -> Int
f1 m n = let n = 5
         in n + m

f2 :: Int -> Int -> Int
f2 m n = n + m
    where n = 5

