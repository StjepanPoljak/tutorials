module Main where

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
        deriving (Eq, Show, Ord)

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
