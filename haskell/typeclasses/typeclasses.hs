module Main where

import Data.Char

data Colors = Black
            | Red
            | Blue
            | Green
        deriving (Eq, Show, Enum)

data Animals = Dog
             | Cat
             | Lizard
        deriving (Eq, Show, Bounded)

data Pet = PetDog String
         | PetCat String
         | Unknown
        deriving (Eq, Show, Read, Ord)

main = do

    putStrLn $ show $ (toEnum 1 :: Colors)
    putStrLn $ show $ fromEnum Red

    putStrLn $ show $ [Black .. Blue]
    putStrLn $ show $ [Red ..]

    putStrLn $ show $ (minBound :: Animals)
    putStrLn $ show $ (maxBound :: Animals)

    putStrLn $ show $ PetDog "Horsey" <= Unknown
    putStrLn $ show $ maximum [ PetDog "Horsey"
                              , PetCat "Panda"
                              , PetCat "Lizard"
                              , PetDog "Doggo" ]

    putStrLn $ show $ (read "PetDog \"Mr. Hyde\"" :: Pet)

-- set of examples for defining instances                   --

    putStrLn $ show $ Good == Good
    putStrLn $ show $ Good <= Intense
    putStrLn $ show $ Horrible > Intense

    putStrLn $ show $ fromEnum Horrible
    putStrLn $ show $ (toEnum 2 :: Smell)
    putStrLn $ show $ (read "gOoD" :: Smell)

data Smell = Good | Horrible | Intense

instance Show Smell where
    show a = case a of
        Good        -> "Nice!"
        Horrible    -> "Ugh!"
        Intense     -> "Interesting..."

instance Read Smell where                   -- rest of the string   --
    readsPrec _ good = case lowerGood of    --      ||              --
                        "good"      -> [(Good,      "")]
                        "horrible"  -> [(Horrible,  "")]
                        "intense"   -> [(Intense,   "")]
                        _           -> []
                where lowerGood = map toLower good

-- Minimum complete definition for Read: readsPrec || readPrec      --
-- Note: readPrec is a proposed replacement for readsPrec           --

instance Eq Smell where
    a == b = case a of
        Good        -> case b of
                        Good        -> True
                        _           -> False

        Horrible    -> case b of
                        Horrible    -> True
                        _           -> False

        Intense     -> case b of
                        Intense     -> True
                        _           -> False

-- Eq is expected to satisfy the following properties:      --
--                                                          --
-- 1) Reflexivity:      x == x                              --
-- 2) Symmetry:         x == y && y == x                    --
-- 3) Transitivity:     x == y && y == z -> x == z          --
-- 4) Substitutivity:   if x == y and f returns an instance --
--                      of Eq, then f x == f y              --
-- 5) Negation:         x /= y => not (x == y)              --
--                                                          --
-- Minimal complete definition: (==) || (/=)                --

instance Ord Smell where
    Good        <= Horrible     = True
    Horrible    <= Intense      = True
-- Transitivity is not implied so it has to be explicit:    --
    Good        <= Intense      = True
    _           <= _            = False

-- Ord is inherited from Eq and is used for totally ordered --
-- data types, i.e. it has to satisfy:                      --
--                                                          --
-- 1) Transitivity:     x <= y && y <= z -> x <= z          --
-- 2) Reflexivity:      x <= x                              --
-- 3) Antisymmetry:     x <= y && y <= x -> x == y          --
--                                                          --
-- Minimal complete definition: (<=) || compare             --
--                                                          --
-- Note: compare is useful for more complex types and is    --
--       defined as:                                        --
--                                                          --
--          compare :: a -> a -> Ordering                   --
--          data Ordering = LT | EQ | GT                    --

instance Enum Smell where
    fromEnum a = case a of
                    Horrible    -> 2
                    Good        -> 1
                    Intense     -> 0
    toEnum a = case a of
                    0           -> Horrible
                    1           -> Good
                    2           -> Intense

-- Minimal complete definition: toEnum && fromEnum          --
-- Note: If a type is both Bounded and Enum then the        --
--       following should hold:                             --
-- 1) succ maxBound && pred minBound generate runtime error --
-- 2) fromEnum and toEnum should give a runtime error if    --
--    the result value is not representable in the result   --
--    type (toEnum 7 :: Bool generates an error)            --
-- 3) enumFrom and enumFromThen should be defined with an   --
--    implicit bound
