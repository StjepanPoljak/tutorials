main :: IO()
main = do printseq
            -- multiple ways to compose a function --
            [ (g . f) [4, 12, 6]
            , g . f $ [4, 12, 6]
            , g . (\lst -> (length lst, sum lst)) $ [4, 12, 6]

            -- we can use ((g .) . f) to first provide two arguments to f   --
            -- apparently, in Data.Composition we can use .: for this       --
            , ((gf .) . h) 4 [4, 3, 2]
            , (gf . (h 4)) [4, 3, 2]
            , (gf . (\(x,y) -> h x y)) (4, [4, 3, 2]) ]

    where gf :: Fractional n => [n] -> n
          gf = g . f

-- the goal is only to demonstrate composition; this should be:     --
-- f :: Fractional n => [n] -> Maybe (Int, n)                       --
-- f [] = Nothing                                                   --
-- f list = Just ...                                                --
f :: Fractional n => [n] -> (Int, n)
f list = fstep list begin
    -- functions can be declared in "where" block --
    where fstep :: Fractional n => [n] -> (Int, n) -> (Int, n)
          fstep [] res = res
          fstep (x:xs) (cnt, sum) = fstep xs (cnt + 1, sum + x)

          begin :: Fractional n => (Int, n)
          begin = (0, 0)

g :: Fractional n => (Int, n) -> n
g (cnt, sum) = sum / (fromIntegral cnt)

h :: Fractional n => Int -> [n] -> [n]
h a (x:xs) = (fromIntegral a * x):xs

printseq :: Show n => [n] -> IO ()
printseq [] = return ()
printseq (x:xs) = do
    putStrLn $ show $ x
    printseq xs
    return ()

