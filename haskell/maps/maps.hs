module Main where

import qualified Data.Map.Lazy as ML
-- Note: Data.Map.Lazy is the default (for Data.Map).       --

import qualified Data.Map.Strict as MS
-- Note: Use Data.Map.Strict only if you will eventually    --
--       need all the values stored in the map or if the    --
--       stored values aren't large virtual data structures --
--       that should be lazily computed.                    --

import qualified Data.IntMap.Lazy as IML                    --
-- Note: This is best if the keys are of type Int.          --

-- If you don't care about ordering, use Data.HashMap.Lazy  --
-- and Data.HashMap.Strict from unordered-containers        --
-- package.                                                 --

-- fromList :: Ord k => [(k, a)] -> Map k a                 --
-- fromSet :: (k -> a) -> Set k -> Map k a                  --
-- toList :: Map k a -> [(k, a)]                            --
-- empty :: Map k a                                         --
-- singleton :: k -> a -> Map k a                           --

-- insert :: Ord k => k -> a -> Map k a -> Map k a          --
-- delete :: Ord k => k -> Map k a -> Map k a               --
-- lookup :: Ord k => k -> Map k a -> Maybe a               --
-- (!?) :: Ord k => Map k a -> k -> Maybe a                 --
-- (!) :: Ord k => Map k a -> k -> a                        --

-- null :: Map k a -> Bool                                  --
-- size :: Map k a -> Int                                   --

-- union :: Ord k => Map k a -> Map k a -> Map k a          --
-- intersection :: Ord k => Map k a -> Map k b -> Map k a   --
-- difference :: Ord k => Map k a -> Map k b -> Map k a     --

import qualified Data.Set as DS

-- This way we can override instance declarations.          --
newtype Map' a b = Map' (MS.Map a b)

main = do
    putStrLn $ show $ MS.null (MS.empty :: MS.Map Int String)
    putStrLn $ show $ MS.size $ MS.singleton "x" 1
    putStrLn $ show $ IML.fromList [(1, "Hello"), (2, "World")]
    putStrLn $ show $ MS.fromSet (\x -> show $ x) $ DS.fromList [1, 2, 3]

    putStrLn $ show $ lst3
    putStrLn $ show $ MS.lookup "z" $ toMap lst3

    putStrLn $ show $ MS.lookup "x" lst1
    putStrLn $ show $ lst1 MS.!? "z"
-- The following would crash the program:                   --
--  putStrLn $ show $ lst1 MS.! "z"                         --

    putStrLn $ show $ MS.union lst1 lst2
    putStrLn $ show $ MS.intersection lst1 lst2
    putStrLn $ show $ MS.difference lst1 lst2

    putStrLn $ show $ MS.delete "y" lst2

    where lst1 = MS.fromList [("x", 1), ("y", 2)]
          lst2 = ML.insert "z" 8 $ ML.singleton "y" 2
          lst3 = Map' $ MS.fromList [("z", 7)]

toMap :: (Ord a) => Map' a b -> MS.Map a b
toMap (Map' map') = map'

instance (Show a, Show b) => Show (Map' a b) where
    show (Map' lst) = map (\x -> case x of
                            '[' -> '{'
                            ']' -> '}'
                            _   -> x)
                    . show $ MS.toList lst
