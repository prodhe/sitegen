{-
 - Site generator
 -
 - From markdown to HTML
-}

module Main where

-- | Import
import System.Directory (doesDirectoryExist, doesFileExist, createDirectory, removeFile,
                         setCurrentDirectory, getDirectoryContents, removeDirectory, copyFile)
import Text.Pandoc (writeHtmlString, readMarkdown, def)
import Text.Regex.PCRE
import Data.Char (toLower)
import Control.Monad (filterM)


-- | Data
type Title = String
type Date  = String
data Page  = Page (Title, Date) String
             deriving (Show)


-- | MAIN
main = do
       srcExists <- doesDirectoryExist "./source"
       if not srcExists
          then putStrLn "Source directory not found."
          else do
               -- get dir content
               setCurrentDirectory "./source"
               content <- getDirectoryContents "."
               -- filter out files and then *.md files
               files <- filterM doesFileExist content
               let markdownFiles = filter (=~ ".md$") files

               if length markdownFiles == 0
                  then putStrLn "No valid markdown files found!"
                  else do
                         -- process each file and save as a Page
                         pages <- mapM (processFile) (markdownFiles)

                         -- jump back, get the standard template, create a menu
                         -- and merge into the template
                         setCurrentDirectory ".."
                         rawTemplate <- readFile "./source/index.tpl"
                         let menu = "<ul>\n" ++ (createMenu pages) ++ "</ul>\n"
                         let finalTemplate = replaceTag rawTemplate "[-MENU-]" menu
                         putStrLn "Created menu"

                         -- clear the "./output" dir of old stuff
                         clearOutputDir
                         putStrLn "Cleared output"
                         putStrLn "---"

                         -- loop through each page and create the HTML files
                         mapM_ (writeOutput finalTemplate) pages
                         
                         -- copy all non *.md files to output
                         let nonMdFiles = [f | f <- files,
                                               not (f `elem` markdownFiles),
                                               not (f == "index.tpl")]
                         mapM (\f -> do
                                      copyFile ("./source/" ++ f) ("./output/" ++ f)
                                      putStrLn ("Copied source/" ++ f)
                             ) nonMdFiles

                         -- announce finish
                         putStrLn "---\nDone"


-- | ACTIONS
--------------

-- | Write page to output directory via template
writeOutput :: String -> Page -> IO ()
writeOutput template (Page (t, d) page) = do
            writeFile filename output
            putStrLn ("Created " ++ filename)
            where
                filename = "output/" ++ (convertTitle t) ++ ".html"
                output = replaceTag (replaceTag template "[-TITLE-]" ("| " ++ t)) "[-MAIN-]" page

-- | Create an HTML menu based on loaded pages
createMenu :: [Page] -> String
createMenu [] = []
createMenu ((Page ("index", _) _):ps) = "" ++ createMenu ps
createMenu ((Page (t, _) _):ps) = "<li><a href=\"./" ++
                                 convertTitle t ++
                                 ".html\">" ++
                                 t ++
                                 "</a></li>\n" ++
                                 createMenu ps

-- | Clear the output directory of all old files
clearOutputDir :: IO ()
clearOutputDir = do
                   y <- doesDirectoryExist "./output"
                   if y
                      then do
                           setCurrentDirectory "./output"
                           content <- getDirectoryContents "."
                           files <- filterM doesFileExist content
                           mapM_ removeFile files
                           setCurrentDirectory ".."
                      else createDirectory "./output"
                             



-- | CONVERSIONS and FILTERS
------------------------------

-- | Convert plain text markdown to plain text HTML
markdownToHTML :: String -> String
markdownToHTML input = writeHtmlString def $ readMarkdown def input

-- | Process an *.md file and turn it into a valid HTML through Pandoc and main template
-- and save it to output folder
processFile :: FilePath -> IO Page
processFile fileName = do
                       content <- readFile (fileName)
                       let output = markdownToHTML content
                       let name = reverse $ drop 3 $ reverse fileName
                       return $ Page (name, "20141228") output

-- | Convert title (Welcome to my 42 page) into formatted string (welcome-to-my-42-page)
convertTitle :: Title -> String
convertTitle [] = []
convertTitle t  = dashed lowercase
                  where
                    dashed = map (\c -> if c == ' ' then '-' else c)
                    lowercase = [x | x <- (map toLower t),
                                     x `elem` ['a'..'z'] ||
                                     x `elem` ['0'..'9'] ||
                                     x == ' ']

-- | Replace a substring within the template page
--            haystack  search    replace
replaceTag :: String -> String -> String -> String
replaceTag [] _ _ = []
replaceTag str search replace = unwords (map func (words str))
                                where
                                  func w = if w == search
                                              then replace
                                              else w
