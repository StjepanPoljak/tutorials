-- readFile :: FilePath -> IO String            --
-- writeFile :: FilePath -> String -> IO ()     --

import Data.Char (toUpper)
-- toUpper :: Char -> Char                      --

main = do
    writeFile outfname . alternate =<< readFile fname
    putStrLn $ "Written to: " ++ outfname
    where fname = "test.txt"
          outfname = map toUpper fname

data CharOrd = CharOrd Char Int

alternate :: String -> String
alternate [] = []
alternate (x:xs) = alternateStep (x:xs) (CharOrd x 0)

    where alternateStep :: String -> CharOrd -> String
          alternateStep [] _ = []
          alternateStep (y:ys) (CharOrd c n)
              |  n `mod` 2 == 0  = (toUpper y):alternateStep ys (CharOrd y (n+1))
              |  otherwise       = y:alternateStep ys (CharOrd y (n+1))


