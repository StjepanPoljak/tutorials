-- (=<<) :: Monad m => (a -> m b) -> m a -> m b         --
-- It unwraps the result of function (m a -> m b) and   --
-- puts it into a function a -> mb.                     --

import System.Environment (getArgs)
-- getArgs :: IO [String]                               --
-- Returns arguments passed via command line (without   --
-- name of executable as first element).                --

import Control.Monad (liftM)
-- liftM :: Monad m => (a1 -> r) -> m a1 -> m r         --
-- Takes a function and a monad, applies that function  --
-- to value within the monad, and returns the new value --
-- wrapped back into monad.                             --

main :: IO ()
main = do
--  Couple of ways to print [String] from IO [String]   --
    putStrLn =<< showM getArgs
    putStrLn =<< (\x -> return $ show x) =<< getArgs
    putStrLn =<< liftM show getArgs

showM :: (Monad m, Show a) => m a -> m String
showM x = do
-- Unwrapping value from monadic context.               --
    y <- x
-- Wrap value back into monadic context.                --
    return $ show y
