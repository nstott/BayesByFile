module Main where

    import System.Environment (getArgs)
    import System.Console.GetOpt
    import System.Exit
    import System.IO
    import qualified System.Directory as D
    import Control.Monad
    import qualified AI.BayesClassifier as B
    import Data.String

    main = do
        args <- getArgs
        let (actions, nonOpts, msgs) = getOpt RequireOrder options args
        opts <- foldl (>>=) (return defaultOptions) actions
        corpus <- train opts
        classify corpus

    classify :: B.Corpus -> IO ()
    classify corpus = do
        forever $ do
            line <- getLine
            putStrLn . show $ B.getBestClassification line corpus
            return ()
        return ()

    --------------------
    -- option processing
    --------------------
    data Options = Options {
        dataDir :: String
        , numGrams :: Int
    } deriving (Show)

    data Flag = Version deriving (Show)

    header = "Usage: classify [OPTION...]"

    options :: [OptDescr (Options -> IO Options)]
    options = [
        Option ['V'] ["version"] (NoArg showVersion) "Show Version Number"
        , Option ['D'] ["data"] (ReqArg setDataDir "PATH") "set the directory containing the training data"
        , Option ['n'] ["number"] (ReqArg setNumGrams "NUM") "set how many items consitute an ngram"]

    defaultOptions :: Options
    defaultOptions = Options {
        dataDir = "./"
        , numGrams = 1
    }

    showVersion :: Options -> IO Options
    showVersion _ = do
        putStrLn "classify 0.1"
        exitWith ExitSuccess

    setDataDir :: String -> Options -> IO Options
    setDataDir path o = return o {dataDir = path}

    setNumGrams :: String -> Options -> IO Options
    setNumGrams num o = return o {numGrams = (read num :: Int)}

    -----------
    -- training
    -----------
    train :: Options -> IO B.Corpus
    train opts = do
        let cleanedPath = appendSlash $ dataDir opts
        classificationNames <- validTrainingDirectories cleanedPath
        files <- fmap concat $ mapM (validTrainingFiles cleanedPath) classificationNames
        text <- mapM (catFile cleanedPath) files
        putStrLn $ "Parsing Training Files from " ++  (show cleanedPath)
        putStrLn $ " - Found " ++ (show $ length classificationNames) ++ " classifications."
        let corpus = foldl (\a b -> B.train (snd b) (fst b) a) (B.newCorpus (numGrams opts) []) text
        putStrLn $ " - " ++ (show $ B.totalDocuments corpus) ++ " documents parsed."
        return corpus

    validTrainingDirectories :: FilePath -> IO [FilePath]
    validTrainingDirectories f = do
        dirs <- D.getDirectoryContents f
        filterM (validDirectory f) dirs

    validTrainingFiles :: FilePath -> FilePath -> IO [(B.ClassificationName, FilePath)]
    validTrainingFiles base dir = do
        let cleanDir = base ++ (appendSlash dir)
        dirs <- D.getDirectoryContents cleanDir
        files <- filterM (validFile cleanDir) dirs
        return $ map (\file -> (dir, file)) files

    catFile :: FilePath -> (B.ClassificationName, FilePath) -> IO (B.ClassificationName, String)
    catFile base (classification, file) = do
        handle <- openFile (base ++ (appendSlash classification) ++ file) ReadMode
        text <- hGetContents handle
        return (classification, text)

    appendSlash :: FilePath -> FilePath
    appendSlash [] = "/"
    appendSlash p
        | last p == '/' = p
        | otherwise = p ++ "/"

    validDirectory :: FilePath -> FilePath -> IO Bool
    validDirectory base dir = do
        existo <- D.doesDirectoryExist $ base ++ dir
        return $ existo && (head dir /= '.')

    validFile :: FilePath -> FilePath -> IO Bool
    validFile base file = do
        existo <- D.doesFileExist $ base ++ file
        return $ existo && isTextFile file

    isTextFile :: FilePath -> Bool
    isTextFile file = (take 4 $ reverse file) == "txt."

