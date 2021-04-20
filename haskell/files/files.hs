-- readFile :: FilePath -> IO String                                    --
-- writeFile :: FilePath -> String -> IO ()                             --
-- lines :: String -> [String]                                          --
-- null :: Foldable t => t a -> Bool                                    --

import Data.Char (toUpper)
-- toUpper :: Char -> Char                                              --

import Control.Monad
-- (=<<) :: Monad m => (a -> m b) -> m a -> m b                         --
-- (<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c             --

main = do
    writeFile outfname . alternate =<< readFile fname
    putStrLn $ "Written to: " ++ outfname

-- Note that the lines function is not portable, as it splits on '\n'.  --
-- Namely, on Windows systems, lines are separated by '\r\n'.           --
    putStrLn <=< liftM show . liftM lines $ readFile fname
    putStrLn =<< show `liftM` lines `liftM` readFile outfname

-- Fish operator (<=<) acts exactly like function composition (in the   --
-- category of endofunctors, of course).                                --
    (putStrLn <=< liftM show) (fileHasEmptyLines outfname)
-- Expression a $ b is equivalent to (a) (b).                           --
    putStrLn <=< liftM show $ fileHasEmptyLines outfname

    where fname = "test.txt"
          outfname = map toUpper fname

alternate :: String -> String
alternate [] = []
alternate (x:xs) = alternateStep (x:xs) (x, 0)

    where alternateStep :: String -> (Char, Int) -> String
          alternateStep [] _ = []
          alternateStep (y:ys) (c, n)
              |  n `mod` 2 == 0  = toUpper y:alternateStep ys (y, n+1)
              |  otherwise       = y:alternateStep ys (y, n+1)

fileHasEmptyLines :: FilePath -> IO Bool
fileHasEmptyLines fpath = liftM (hasEmptyLines . lines) (readFile fpath)

    where hasEmptyLines :: [String] -> Bool
          hasEmptyLines [] = False
          hasEmptyLines (x:xs)  = null x || hasEmptyLines xs
