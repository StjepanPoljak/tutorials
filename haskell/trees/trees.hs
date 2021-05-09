module Main where

import Data.Tree
-- drawTree :: Tree String -> String                            --
-- flatten :: Tree a -> [a]                                     --

import Control.Monad
-- mapM :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)         --
-- mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()              --
-- foldM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b     --
-- foldM_ :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m ()   --

import System.Directory
-- getCurrentDirectory :: IO FilePath                           --
-- listDirectory :: FilePath -> IO [FilePath]                   --
-- makeRelativeToCurrentDirectory :: FilePath -> IO FilePath    --
-- takeFileName :: FilePath -> FilePath                         --

import System.FilePath
-- joinPath :: [FilePath] -> FilePath                           --

main = do
    putStrLn =<< liftM drawTree fileTree

    putStrLn <=< liftM drawTree
               $ mapM (\x -> return (takeFileName x)) =<< fileTree

    putStrLn =<< liftM (show . flatten) fileTree

    where fileTree = makeTree =<< makeRelativeToCurrentDirectory
                              =<< getCurrentDirectory

makeTreeFold :: Tree FilePath -> FilePath -> IO (Tree FilePath)
makeTreeFold (Node val subTree) curr = do
    isDir       <- doesDirectoryExist newPath
    case isDir of
        False   -> return (Node val (Node newPath []:subTree))
        True    -> (\x -> return $ Node val (x:subTree)) =<< makeTree newPath
    where newPath = joinPath [ val, curr ]

makeTree :: FilePath -> IO (Tree FilePath)
makeTree curr = foldM makeTreeFold (Node curr []) =<< listDirectory curr

