{-
 - File.hs
 -
 - Get directory structure and files
 -
-}

module Filesystem where

-- | Import
import System.Directory
import Control.Monad (filterM)


-- | Data
data File     = File FName FType FContent
type FName    = String
type FType    = String
type FContent = String

data Directory  = Directory DirName DirContent
type DirName    = String
type DirContent = (IO [Directory], IO [File])


-- | Access functions
getDirContent :: DirName -> Directory
getDirContent dir = Directory dir (getDirs dir, getFiles dir)
                    where
                      getDirs dir  = do content <- getDirectoryContents dir
                                        dirs <- filterM doesDirectoryExist content
                                        return $ map makeDir $ filter (\d -> if d == "." || d == ".." then False else True) dirs
                      getFiles dir = do content <- getDirectoryContents dir
                                        files <- filterM doesFileExist content
                                        mapM makeFile files
                      makeDir d    = getDirContent d
                      makeFile f   = do fc <- readFile f
                                        return $ File f makeFileType fc
                      makeFileType = ""

showDirContent :: Directory -> IO ()
showDirContent (Directory dir (subdirs, files)) = do fs <- files
                                                     let fsNames = map getFileName fs
                                                     putStr (dir ++ ": ")
                                                     mapM_ (putStr . (++ " ")) fsNames
                                                     putStrLn ""
                                                     ds <- subdirs
                                                     if not $ null ds
                                                        then mapM_ showDirContent ds
                                                        else return ()

getFileName :: File -> String
getFileName (File fn _ _) = fn
