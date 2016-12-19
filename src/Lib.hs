-- This file is commented extensively for non-haskell programmers

-- | These are language extensions. Haskell has a great many language
-- extensions but in practice you do not need to knwo much about them. If you
-- use a library that needs them, then the library documentation will tell you which
-- extensions you neeed to include. If you try to write code that needs particular extensions,
-- then the haskell compiler is smart enough typically to be able to suggest which extensions
-- you should switch on by including an entry here.

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}
-- | Haskell code is structured as sets of functions that sit within Modules. The basic rule is that a module with a
-- particular name (for example Lib) sits within a .hs file of the same name (eg. Lib.hs). The module statement is of
-- the form `module MODULE_NAME (EXPORTED_FUNCTIONS) where`. Everything following this is part of the module. There are
-- no brackets or any other syntax to worry about.
module Lib
    ( startApp
    ) where

-- | Imports work like most other languages and are essentially library includes. The functions of the lirbary become
-- immediately accessible in the code of the module. There are various ways in which imports can be modified. For
-- example, one may `import qualified X as Y` which imports a library in such a way that the functions of the library
-- must be prefixed with `Y.`. One can always prefix a libraries functions with the import string, when calling them.
-- You will occasionally have reason to import libraries that have common function names by coincidence. You can use
-- qualified imports of full prefixes to disambiguate. The compiler will tell you where the problem is if this occurs.

import           Control.Monad                (when)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Except   (ExceptT)
import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson.Generic
import qualified Data.ByteString.Lazy         as L
import qualified Data.List                    as DL
import           Data.Maybe                   (catMaybes)
import           Data.Text                    (pack, unpack)
import           Data.Time.Clock              (UTCTime, getCurrentTime)
import           Data.Time.Format             (defaultTimeLocale, formatTime)
import           Database.MongoDB
import           GHC.Generics
import           Network.HTTP.Client          (defaultManagerSettings,
                                               newManager)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger
import           RestClient
import           Servant
import qualified Servant.API                  as SC
import qualified Servant.Client               as SC
import           System.Environment           (getArgs, getProgName, lookupEnv)
import           System.Log.Formatter
import           System.Log.Handler           (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Handler.Syslog
import           System.Log.Logger
import           System.Process

import           Argon
import           System.Console.Docopt
import           Pipes
import           Pipes.Safe (runSafeT)
import qualified Pipes.Prelude as P
import           Control.Monad (forM_,forM)
import           System.Directory (getDirectoryContents)
import           System.Directory (doesFileExist)
import           System.Directory (listDirectory)
import           System.FilePath.Posix
import           Data.List (isSuffixOf)
import           Data.List.Split
import           Network.HTTP(simpleHTTP,getRequest,getResponseBody,getResponseCode,ResponseCode)
-- | The Servant library has a very elegant model for defining a REST API. We shall demonstrate here. First, we shall
-- define the data types that will be passed in the REST calls. We will define a simple data type that passes some data
-- from client to the server first. There is nothing special about the data being passed - this is a demonstration
-- only. data types are kind of like structs in C or C++.

data Message = Message { name    :: String
                       , message :: String
                       } deriving (Generic, FromJSON, ToBSON, FromBSON)

deriving instance FromBSON String  -- we need these as BSON does not provide
deriving instance ToBSON   String

-- | We will also define a simple data type for returning data from a REST call, again with nothing special or
-- particular in the response, but instead merely as a demonstration.

data ResponseData = ResponseData { response :: String
                                 } deriving (Generic, ToJSON, FromJSON,FromBSON)

-- | Next we will define the API for the REST service. This is defined as a 'type' using a special syntax from the
-- Servant Library. A REST endpoint is defined by chaining together a series of elements in the format `A :> B :> C`. A
-- set of rest endpoints are chained in the format `X :<|> Y :<|> Z`. We define a set of endpoints to demonstrate
-- functionality as described int he README.md file below.
--
-- Note in the API below that we can mix GET and Post methods. The type of call is determined by the last element in the
-- :> chain. If the method is Get, then the set of QueryParams determine the attributes of the Get call. If the method
-- is Post, then there will be a single ReqBody element that defines the type being transmitted. The return type for
-- each method is noted in the last element in the :> chain.

data Author = Author { username :: String
                 } deriving (Generic, FromJSON, ToJSON, FromBSON)


type API = "load_environment_variables" :> QueryParam "name" String :> Get '[JSON] ResponseData
      :<|> "getREADME"                  :> Get '[JSON] ResponseData
      :<|> "authors"                    :> Get '[JSON] [Author]
      :<|> "author"                     :> Capture "name" String:> Get '[JSON] Author
      :<|> "analysisAllTheRepo"         :> Get '[JSON] [(FilePath, AnalysisResult)]
      :<|> "fetchRepo"                  :> QueryParam "url" String:> Get '[JSON] FetchResponseMsg
      :<|> "methodComplexity"           :> QueryParam "url" String:> Get '[JSON] [(FilePath, AnalysisResult)]
      :<|> "fileComplexity"             :> QueryParam "url" String:> Get '[JSON] [(FilePath, Double)]
      :<|> "projectComplexity"          :> QueryParam "url" String:> Get '[JSON] (String,Double)

data FetchResponseMsg = FetchResponseMsg { msg :: String} deriving (Eq, Show)

$(deriveJSON defaultOptions ''FetchResponseMsg)


-- | Now that we have the Service API defined, we next move on to implementing the service. The startApp function is
-- called to start the application (in fact there is a main function in the file ../app/Main.hs, which is the entry
-- point to the executable, but by convention, it calls startApp in this file. startApp in turn calls a function `app`
-- defined below.
--
-- startApp is defind below on two lines. The first line is a type definition. It states that startApp returns no value
-- (denoted by `()`), but performs Input/Output. Haskell programmes are divided into code that performs I/O and code
-- that does not. The separation is very useful because I/O bound code can return different results even when passed the
-- same parameters (think about it) but non-I/O code will always return the same values for the same parameters. This
-- means we can reason about the correctness of non-I/O code more rigourously, and so the distinction is
-- worthwhile. However, the mixing of I/O and non-I/O code is one of the key problems beginning Haskell programmers have
-- with development of systems. If one fails to annotate type signatures correctly with `IO`, or write IO code
-- incorrectly, the type errors that the compiler generates are quite hard to decypher untill you understand how IO code
-- works. Firtunately its quite simple, and we shall explain below in function definitions.
--
-- A C++, Java or C programmer (amongst others) is used to a formatting of a function or method call of the form `A (X,
-- Y, Z)` where X, Y and Z are parameters passed to the execution of A. Haskell does not use the brackets, such that the
-- app function is defined entirely as a call to the serve function, passing it api and server. Note that these
-- parameters and both functions. In Haskell you can treat functions in an equivalent way to data.
--
-- Note that I have modified the standard implementation of startApp to include logging support, which is explained the
-- the text accompanying the function `withLogging` towards the end of this file.
startApp :: IO ()    -- set up wai logger for service to output apache style logging for rest calls
startApp = withLogging $ \ aplogger -> do

  warnLog "Starting use-haskell."

  let settings = setPort 8080 $ setLogger aplogger defaultSettings
  runSettings settings app

--startApp :: IO ()
--startApp = run 8080 app

-- | the function app calls serve, passing api and server. You can see that there is a kind of structure of function
-- composition here that is to do with how servant operates. The first parameter defines the type of the REST service,
-- (which is defined as type `Proxy API` - yes types of this formt are allowed - and we should note that the second part
-- of that type is out previously defined REST API) and the second parameter defines the implementation of the REST service.
app :: Application
app = serve api server

api :: Servant.Proxy API
api = Proxy

-- | And now we implement the REST service by matching the API type, providing a Handler method for each endpoint
-- defined in the API type. Normally we would implement each endpoint using a unique method definition, but this need
-- not be so. To add a news endpoint, define it in type API above, and add and implement a handler here.
server :: Server API
server = loadEnvironmentVariable
    :<|> getREADME
    :<|> authors
    :<|> author
    :<|> analysisAllTheRepo
    :<|> fetchRepo
    :<|> methodComplexity
    :<|> fileComplexity
    :<|> projectComplexity

  where

    author::String->Handler Author
    author name =liftIO $ do
      --isFileExist <- doesFileExist "src/Lib.hs"
      --putStrLn $ show $ fileList
      --result <- listDirectory "./"
      --putStrLn $ show $ result
      result<-computeProjectLevelAvg "./src"
      putStrLn $ show result
      return $ Author name

    authors::Handler [Author]
    authors = return [Author "aideen", Author "kanika", Author "shifan", Author "andrew"]

    -- user::String->Handler User
    -- user name = return $ User name "1"


    fetchRepo :: Maybe String -> Handler FetchResponseMsg
    fetchRepo url = liftIO $ do
        case url of
          Nothing -> return $ FetchResponseMsg "aaagggghh!"
          Just url' -> do
              let url = url'
              let gitName = last $ splitOn "/" url
              let name = head $ splitOn "." gitName
              callCommand $ "git clone " ++ url' ++ " repo/"++name
              --results<-computeMethodLevelAvg "./"
              --return (concat results)
              return $ FetchResponseMsg $ "great success"

    methodComplexity :: Maybe String -> Handler [(FilePath, AnalysisResult)]
    methodComplexity url = liftIO $ do
        case url of
          Nothing -> return $ error "Something Wrong"
          Just url' -> do
              callCommand $ "git clone " ++ url'
              let url = url'
              let gitName = last $ splitOn "/" url
              let name = head $ splitOn "." gitName
              results <- computeMethodLevelAvg ("./" ++ name)
              return results

    fileComplexity :: Maybe String -> Handler [(FilePath, Double)]
    fileComplexity url = liftIO $ do
        case url of
          Nothing -> return $ error "Something Wrong"
          Just url' -> do
              callCommand $ "git clone " ++ url'
              let url = url'
              let gitName = last $ splitOn "/" url
              let name = head $ splitOn "." gitName
              results <- computeFileLevelAvg ("./" ++ name)
              return results

    projectComplexity :: Maybe String -> Handler (String,Double)
    projectComplexity url = liftIO $ do
        case url of
          Nothing -> return $ error "Something Wrong"
          Just url' -> do
              callCommand $ "git clone " ++ url'
              let url = url'
              let gitName = last $ splitOn "/" url
              let name = head $ splitOn "." gitName
              result <- computeProjectLevelAvg ("./" ++ name)
              return (name,result)


    analysisAllTheRepo :: Handler [(FilePath, AnalysisResult)]
    analysisAllTheRepo =liftIO $ do
      --results<-computeMethodLevelAvg "./repo"
      return results


    computeMethodLevelAvg :: FilePath ->IO [(FilePath, AnalysisResult)]
    computeMethodLevelAvg path = do

      files <-traverseDir path  
      results<-forM (filterHaskellFile files) $ \path -> do
        (file,analysis) <- analyze getConfig path
        return [(file,analysis)]
      return (concat results)

    computeFileLevelAvg :: FilePath -> IO [(FilePath, Double)]
    computeFileLevelAvg path = liftIO $ do
      input <- computeMethodLevelAvg path
      avgOfBlock<-forM input $ \block -> do 
        avgOfFile<-forM (snd block) $ \complexityBlock -> do
          complexityList<-forM complexityBlock $ \cc -> do
            return $ getComplexity cc 
          let a = sum complexityList
          let b = length complexityList
          let c = fromIntegral a / fromIntegral b
          return (fst block,c)
        return avgOfFile
      filteredResult <-forM avgOfBlock $ \a -> do 
        case a of Right ele -> return ele
                  Left ele -> return ("undefined",0.0)
      return $ filteredResult



    computeProjectLevelAvg :: FilePath -> IO Double
    computeProjectLevelAvg path = do 
      result <- computeFileLevelAvg path
      complexityList<- forM result $ \complexityTuple -> do
        putStrLn $ show (snd complexityTuple)
        return (snd complexityTuple)
      return (sum $ filter (not.isNaN) complexityList)


    getComplexity :: ComplexityBlock -> Int
    getComplexity ( CC tuple ) = getComplexity' tuple

    getComplexity' :: (Loc, String, Int) -> Int
    getComplexity' ( _,_,complexity ) = complexity


    loadEnvironmentVariable :: Maybe String -> Handler ResponseData
    loadEnvironmentVariable ms = liftIO $ do
      warnLog $ "request to load environment variable: " ++ show ms
      -- Something that is of type Maybe String will either have a value Nothing
      -- or a value Just s, where s is a String. The following case statement allows us to
      -- distinguish between the two possibilities and act accordingly. If we get nothing in this case,
      -- we throw an exception. Otherwise, we look for the environment variable, throwing an exception
      -- if it is not set (probably not sensible program behaviour but this is just a demonstration).
      --
      -- Note that this function has deliberately been written in an empirical style that would be familiar to
      -- Java of C or C++ programmers, essentially as a kind of long drawn out if/else structure (although in fact
      -- written with a case statement structure). See the searchMessage implementation below for a more elegant and
      -- therefore less complex implementation, that takes some advantage of Haskell coding style.
      case ms of
        Nothing -> do
          warnLog "No environment variable requested"
          return $ ResponseData "WAT? No environment variable requested."
        Just s  -> liftIO $ do
          e <- lookupEnv s -- lookupEnv is an IO function, so we must use the `<-` notation
          case e of
            -- If the environment variable is not set, then create a lof entry and return and exception
            Nothing -> do
              warnLog $ "Environment variable " ++ s ++ " is not set."
              return $ ResponseData $  "Environment variable " ++ s ++ " is not set."

            -- Otherwise, return the envrionment variable. Note how variable names can use ', eg. x', x''
            -- Haskell programmers often use this format for variables that are essentially referring to the same thing
            Just e' -> return $ ResponseData e'

    -- |  One can do File IO with the standard functions:
    --                 readFile  :: FilePath -> IO String
    --                 writeFile :: FilePath -> String -> IO ()
    -- we shall set the location of the README file to return using a command line argument
    -- So our first task is to get the README file location, then open it and return its contents to the client.


    -- the getREAME function below is equialent to the following
    -- pseudo-java euivalent type:
    --   ResponseData getREADME ( ){
    --         // code goes here
    --   }

    getREADME, getREADME' :: Handler ResponseData -- fns with no input, second getREADME' is for demo below
    getREADME = liftIO $ do
      [rPath] <- getArgs         -- alternatively (rPath:xs) <- getArgs
      s       <- readFile rPath
      return $ ResponseData s

    -- here is an alternative implementation of getREADME, more in keeping the Haskell style
    -- takes a bit of practice, but very easy to read, if yuo understand the symbols, and very hard to get wrong.
    -- in english, read file idenfitied by the head of the argument list and return as a ResponseData structure
    getREADME' = liftIO $ ResponseData <$> (readFile . head =<< getArgs)


-- | error stuff
custom404Error msg = err404 { errBody = msg }


-- | Logging stuff
iso8601 :: UTCTime -> String
iso8601 = formatTime defaultTimeLocale "%FT%T%q%z"

-- global loggin functions
debugLog, warnLog, errorLog :: String -> IO ()
debugLog = doLog debugM
warnLog  = doLog warningM
errorLog = doLog errorM
noticeLog = doLog noticeM

doLog f s = getProgName >>= \ p -> do
                t <- getCurrentTime
                f p $ (iso8601 t) ++ " " ++ s

withLogging act = withStdoutLogger $ \aplogger -> do

  lname  <- getProgName
  llevel <- logLevel
  updateGlobalLogger lname
                     (setLevel $ case llevel of
                                  "WARNING" -> WARNING
                                  "ERROR"   -> ERROR
                                  _         -> DEBUG)
  act aplogger




-- | Determines log reporting level. Set to "DEBUG", "WARNING" or "ERROR" as preferred. Loggin is
-- provided by the hslogger library.
logLevel :: IO String
logLevel = defEnv "LOG_LEVEL" id "DEBUG" True


-- | Helper function to simplify the setting of environment variables
-- function that looks up environment variable and returns the result of running funtion fn over it
-- or if the environment variable does not exist, returns the value def. The function will optionally log a
-- warning based on Boolean tag
defEnv :: Show a
              => String        -- Environment Variable name
              -> (String -> a)  -- function to process variable string (set as 'id' if not needed)
              -> a             -- default value to use if environment variable is not set
              -> Bool          -- True if we should warn if environment variable is not set
              -> IO a
defEnv env fn def doWarn = lookupEnv env >>= \ e -> case e of
      Just s  -> return $ fn s
      Nothing -> do
        when doWarn (doLog warningM $ "Environment variable: " ++ env ++
                                      " is not set. Defaulting to " ++ (show def))
        return def



getConfig ::Config
getConfig = Config {
      minCC       = read "2"
    , exts        = []
    , headers     = []
    , includeDirs = []
    , outputMode  = JSON
    }

getPath::[String]
getPath = ["src"]



traverseDir :: FilePath -> IO [FilePath]
traverseDir top = do
  ds <- listDirectory top
  paths <- forM ds $ \d -> do
    let path = top </> d
    isFile<-liftIO $ doesFileExist path
    if not isFile
      then traverseDir path
      else return [path]
  return (concat paths)

filterHaskellFile :: [FilePath] ->[FilePath]
filterHaskellFile fileList = filter (".hs" `isSuffixOf`) fileList


filterFiles :: FilePath ->IO [FilePath]
filterFiles path = do
  root <- listDirectory path
  --putStrLn $ show root
  --
  printList path root
  return root

  where

    printList :: FilePath->[FilePath] -> IO ()
    printList base root = do
      forM_ root $ \name -> do
        let b = (base </> name)
        isFile<- liftIO $ doesFileExist $ base </> name
        if not isFile then do
                        result <- listDirectory $ base </> name

                        printList (base </> name) result
                        --putStrLn $ show a
                      else do
                        putStrLn $ show "hello"
                        --putStrLn $ base </> name

-- http request
getHttpR :: String -> IO String
getHttpR url = simpleHTTP (getRequest url) >>= getResponseBody

getCode :: String -> IO ResponseCode
getCode url = simpleHTTP req >>= getResponseCode
    where req = getRequest url
