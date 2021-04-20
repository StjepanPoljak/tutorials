-- map :: (a -> b) -> [a] -> [b]                                --
-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b        --
-- foldl' :: Foldable t => (b -> a -> b) -> b -> t a -> b       --
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b        --

import Control.Monad

-- import Data.Sequence

main = do

    putStrLn $ show $ mylist

    putStrLn $ show $ foldr (\x acc -> show x ++ acc) "" [1, 2, 3, 4]
    putStrLn $ show $ foldr (\x acc -> show x ++ acc) "" mylist

-- The result here is "1234" or:                                        --
-- show 1 ++ (show 2 ++ (show 3 ++ (show 4)))                           --
-- This is basically most optimal for most applications, as it          --
-- reflects the construction of the list (it "transforms" the list):    --
-- MyL 1 (MyL 2 (MyL 3 (MyL 4)))                                        --

    putStrLn $ show $ foldl (\acc x -> show x ++ acc) "" [1, 2, 3, 4]

-- Here we have "4321", so reverse of the previous example:             --
-- (((show 4) ++ show 3) ++ show 2) ++ show 1                           --
-- As Haskell is lazy, it will not evaluate this unless there is        --
-- demand, and it has to store thunks, thus taking up space. There are	--
-- no guarantees as with foldr.                                         --

    where mylist = MyL 1 (MyL 2 (MyL 3 (MyL 4 (Empty))))

data MyList a = MyL a (MyList a) | Empty deriving (Show, Eq)

-- Disregard this for now.                                              --
instance Foldable MyList where
    foldMap f Empty = mempty
    foldMap f (MyL x xs) = f x <> foldMap f xs
