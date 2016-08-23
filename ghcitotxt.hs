-- file: ghcitotxt.hs
import System.IO
import System.Process (readProcess)
import qualified Data.String.Utils as SU
import Data.List (findIndices, findIndex)
import Data.Maybe (fromJust, isJust)
import Data.IORef
import Options.Applicative
import Data.Monoid

-- functions used to find lines generating an output

firstClosingBracketAfter :: Int -> [String] -> Maybe Int
firstClosingBracketAfter i list = if(isJust index) then Just $ (fromJust index) + length list1 else Nothing
                                     where (list1,list2) = splitAt i list
                                           index = findIndex (\x -> SU.startswith ":}" x) list2

firstOpeningBracketBefore :: Int -> [String] -> Maybe Int
firstOpeningBracketBefore i list = if(isJust index) then Just $ length list2 - 1 - (fromJust index) else Nothing
                                     where (list1,list2) = splitAt (length list - i - 1) (reverse list)
                                           index = findIndex (\x -> SU.startswith ":{" x) list2

lineBelongsToMultiBlock :: Int -> [String] -> Bool
lineBelongsToMultiBlock i list = if isJust firstCBafter
                                   then firstOBbefore == firstOpeningBracketBefore (fromJust firstCBafter) list
                                   else False
                                     where firstOBbefore = firstOpeningBracketBefore i list
                                           firstCBafter = firstClosingBracketAfter i list

prefixesToDetect = ["import ", "let ", "data ", "where ", "--", "{-#", ":s", ":u", ":l", ":m", ",", "<", "{", "}", "$", ")"]

beginByKeyWord :: String -> [String] -> Bool
beginByKeyWord string keywords = foldr (||) False $ map (\x -> SU.startswith x string) (keywords ++ prefixesToDetect)

isOutput :: [String] -> [Int]
isOutput strings = findIndices (==False) tests
                    where tests = map (\i -> ((tstrings !! i) == "") || (beginByKeyWord ((tstrings !! i)) prefixesToDetect) || (lineBelongsToMultiBlock i tstrings)) [0..((length tstrings)-1)]
                          tstrings = map SU.lstrip strings
              
-- format the hs file given as lines
formatHsfile :: [FilePath] -> [String]
formatHsfile hsfile = (SU.split "\n" ((SU.strip $ SU.join "\n" hsfile))) ++ [""]
                          

-- get the outputs of a script
getGHCIoutputs :: FilePath -> Maybe String -> IO(String)
getGHCIoutputs hsfile packages =
  do
    outputs <- readProcess "ghc" ((if isJust packages then ["-package-db", fromJust packages] else []) ++ ["-dppr-cols140", "-e", ":script " ++ hsfile]) ""
    return ( outputs )

-- add a span for the prompt 
promptInputs :: [String] -> [String]
promptInputs txt = map (\x -> "> " ++ x) txt


-- read several files 
getFiles :: [FilePath] -> IO([[String]])
getFiles files = 
  do 
    mapM (\file -> fmap lines (readFile file)) files


-- insert something at given positions

insertAt :: Int -> a -> [a] -> [a]
insertAt n x xs = as ++ (x:bs)
                  where (as,bs) = splitAt n xs

insertAtIndices :: [Int] -> a -> [a] -> [a]
insertAtIndices (i:is) element list | is == [] = x ++ list2
                                    | otherwise = x ++ (insertAtIndices (map (\j -> j-(length(list1))) is) element list2)  
                                        where x = insertAt i element list1
                                              (list1,list2) = splitAt i list

insertAtIndices2 :: [Int] -> [a] -> [a] -> [a]
insertAtIndices2 indices elements list | indices == [] = list
                                       | is == [] = x ++ list2
                                       | otherwise = x ++ (insertAtIndices2 (map (\j -> j-(length(list1))) is) elements2 list2)  
                                          where x = insertAt i element1 list1
                                                (list1,list2) = splitAt i list
                                                (element1:elements2) = elements
                                                (i:is) = indices


-- ~~## Case multiline outputs ##~~ -- 

-- split the input file
writeTruncatedFiles :: FilePath -> IO( ([FilePath], [Int]) )
writeTruncatedFiles hsfile =
  do
    fileLines <- fmap lines (readFile hsfile)
    let formattedLines = formatHsfile fileLines
    writeFormattedFile <- writeFile "/tmp/hsfile.hs" $ SU.join "\n" formattedLines
    let breaks = isOutput formattedLines
    let tmpfiles = map (\i -> "/tmp/hsfile_" ++ (show i) ++ ".txt") [0..(length breaks - 1)]
    writefiles <- mapM_ (\i -> writeFile (tmpfiles !! i) (SU.join "\n" $ take (breaks !! i + 1) formattedLines)) [0..(length breaks - 1)]
    return (tmpfiles, breaks)

getListOutputs :: [FilePath] -> IO([[String]])
getListOutputs outputfiles = 
  do
    filesLines <- getFiles outputfiles
    let f = \i -> if i==0 then filesLines !! 0 else drop ((length $ filesLines !! (i-1))-1) (filesLines !! i) 
    return (map (init.f) [0..(length outputfiles -1)])

mainFun :: FilePath -> Maybe String -> IO ( ([[String]], [Int]) )
mainFun hsfile packages =
  do
    (tmpfiles, breaks) <- writeTruncatedFiles hsfile
    let outputfiles = map (\i -> "/tmp/ghcOutput_" ++ (show i) ++ ".txt") [0..(length tmpfiles - 1)]
    i <- newIORef 0
    mainLoop i tmpfiles outputfiles packages
    listOutputs <- getListOutputs outputfiles
    return (listOutputs, breaks)

mainLoop :: IORef Int -> [FilePath] -> [FilePath] -> Maybe String -> IO ()
mainLoop i tmpfiles outputfiles packages =
  do 
    let nfiles = length tmpfiles
    j <- readIORef i
    if j >= nfiles
      then return ()
      else
        do
          let hsfile = tmpfiles !! j
          let outfile = outputfiles !! j
          outhi <- openFile outfile WriteMode
          output <- getGHCIoutputs hsfile packages
          hPutStrLn outhi output
          hClose outhi
          modifyIORef i (+1)    -- increase it by 1
          mainLoop i tmpfiles outputfiles packages

scriptToTxtWithOutputs :: FilePath -> Maybe String -> IO()
scriptToTxtWithOutputs hsfile packages = 
  do
    (outputs, breaks) <- mainFun hsfile packages
    let indices = map (+1) $ breaks
    let listOutputs = map (\x -> SU.join "\n" x) outputs
    txt <- fmap lines (readFile "/tmp/hsfile.hs")
    let basefilename = last $ SU.split "/" hsfile
    let txtWithInsertions = SU.join "\n" 
                              $ insertAtIndices2 indices listOutputs 
                                $ promptInputs txt 
--    let outfilename = "output_" ++ ((SU.split "." basefilename) !! 0) ++ ".txt"
--    writeFile outfilename txtWithInsertions
    putStrLn txtWithInsertions

-- ~~## Case monoline outputs ##~~ -- 

getOutputsAndBreaks :: FilePath -> Maybe String -> IO( [String], [Int] )
getOutputsAndBreaks hsfile packages = 
  do
    fileLines <- fmap lines (readFile hsfile)
    let formattedLines = formatHsfile fileLines
    writeFormattedFile <- writeFile "/tmp/hsfile.hs" $ SU.join "\n" formattedLines
    let breaks = isOutput formattedLines
    runGHC <- getGHCIoutputs "/tmp/hsfile.hs" packages
    let outputs = SU.split "\n" runGHC
    return (outputs, breaks)

scriptToTxtWithMonoOutputs :: FilePath -> Maybe String -> IO()
scriptToTxtWithMonoOutputs hsfile packages = 
  do
    (outputs, breaks) <- getOutputsAndBreaks hsfile packages
    let indices = map (+1) $ breaks
    txt <- fmap lines (readFile "/tmp/hsfile.hs")
    let basefilename = last $ SU.split "/" hsfile
    let txtWithInsertions = SU.join "\n" 
                               $ insertAtIndices2 indices outputs 
                                 $ promptInputs txt 
    let outfilename = (SU.split "." basefilename) !! 0 ++ ".html"
--    writeFile outfilename txtWithInsertions
    putStrLn txtWithInsertions
    

-- ~~## Parser ##~~ --

data Arguments = Arguments
  { infile :: String
  , monoline :: Bool
  , packages :: Maybe String }

scriptToHtml :: Arguments -> IO()
scriptToHtml (Arguments infile False packages) = do scriptToTxtWithOutputs infile packages
scriptToHtml (Arguments infile True  packages) = do scriptToTxtWithMonoOutputs infile packages

run :: Parser Arguments
run = Arguments
     <$> argument str 
          ( metavar "FILE"
         <> help "The Haskell script" )
     <*> switch
          ( long "monooutputs"
         <> short 'm'
         <> help "In case every output takes only one line" )
     <*> ( optional $ strOption 
          ( metavar "package database"
         <> long "package-db"
         <> short 'p'
         <> help "Add the package database" ))

main :: IO ()
main = execParser opts >>= scriptToHtml
  where
    opts = info (helper <*> run)
      ( fullDesc
     <> progDesc "Convert a Haskell script to html"
     <> header "ghcitohtml - based on HsColour" )

