{-#LANGUAGE BangPatterns#-}

import Numeric
import Data.Word
import Data.List
import Data.Char (ord,isSpace)
import Data.Bits
import qualified Control.Exception as E
import Control.Applicative
import Control.Monad
import System.Posix.Files
import System.Posix.Types
import System.Directory
import System.FilePath
import System.Environment
import System.Exit
import System.IO

-- copy from https://github.com/haskell/cabal/blob/master/cabal-install/Distribution/Client/Sandbox.hs
sandboxBuildDir :: FilePath -> FilePath
sandboxBuildDir sandboxDir = "dist/dist-sandbox-" ++ showHex sandboxDirHash ""
  where
    sandboxDirHash = jenkins sandboxDir

    -- See http://en.wikipedia.org/wiki/Jenkins_hash_function
    jenkins :: String -> Word32
    jenkins str = loop_finish $ foldl' loop 0 str
      where
        loop :: Word32 -> Char -> Word32
        loop hash key_i' = hash'''
          where
            key_i   = toEnum . ord $ key_i'
            hash'   = hash + key_i
            hash''  = hash' + (shiftL hash' 10)
            hash''' = hash'' `xor` (shiftR hash'' 6)

        loop_finish :: Word32 -> Word32
        loop_finish hash = hash'''
          where
            hash'   = hash + (shiftL hash 3)
            hash''  = hash' `xor` (shiftR hash' 11)
            hash''' = hash'' + (shiftL hash'' 15)



-- copy from https://hackage.haskell.org/package/cab-0.2.14/docs/src/Distribution-Cab-Sandbox.html#getSandbox

configFile :: String
configFile = "cabal.sandbox.config"

pkgDbKey :: String
pkgDbKey = "package-db:"

pkgDbKeyLen :: Int
pkgDbKeyLen = length pkgDbKey

-- | Find a sandbox config file by tracing ancestor directories,
--   parse it and return the package db path
getSandbox :: IO (Maybe FilePath)
getSandbox = (Just <$> getPkgDb) `E.catch` handler
  where
    getPkgDb = getCurrentDirectory >>= getSandboxConfigFile >>= getPackageDbDir
    handler :: E.SomeException -> IO (Maybe String)
    handler _ = return Nothing

-- | Find a sandbox config file by tracing ancestor directories.
--   Exception is thrown if not found
getSandboxConfigFile :: FilePath -> IO FilePath
getSandboxConfigFile dir = do
    let cfile = dir </> configFile
    exist <- doesFileExist cfile
    if exist then
        return cfile
      else do
        let dir' = takeDirectory dir
        if dir == dir' then
            E.throwIO $ userError "sandbox config file not found"
          else
            getSandboxConfigFile dir'

-- | Extract a package db directory from the sandbox config file.
--   Exception is thrown if the sandbox config file is broken.
getPackageDbDir :: FilePath -> IO FilePath
getPackageDbDir sconf = do
    -- Be strict to ensure that an error can be caught.
    !path <- extractValue . parse <$> readFile sconf
    return path
  where
    parse = head . filter ("package-db:" `isPrefixOf`) . lines
    extractValue = fst . break isSpace . dropWhile isSpace . drop pkgDbKeyLen

type ProjectRootDir = FilePath
type BinaryName = FilePath


getTimeStamp :: FilePath -> IO (Either E.SomeException EpochTime)
getTimeStamp path = do
  stat <- E.try $ getFileStatus path
  return $ fmap modificationTime stat

searchBinary :: ProjectRootDir -> BinaryName -> IO FilePath
searchBinary rdir name = do
  timeT <- getTimeStamp distTBin
  msdir <- getSandbox
  case msdir of
    Just sdir -> do
      let distS = sandboxBuildDir $ takeDirectory sdir
      let distSBin = rdir </> distS </> "build" </> name </> name
      timeS <- getTimeStamp distSBin
      case (timeS,timeT) of
        (Right s,Right t) -> return $ if t <= s then distSBin else distTBin
        (Right _,Left _) -> return distSBin
        (Left _,Right _) -> return distTBin
        _ -> errorHandler [distSBin,distTBin]
    Nothing -> do
      case timeT of
        (Right _) -> return distTBin
        _ -> errorHandler [distTBin]
  where
    distTBin = rdir </> "dist" </> "build" </> name </> name
    errorHandler paths = do
      forM_ paths $ \path -> do
        hPutStrLn stderr $ "Check:" ++ path
      hPutStrLn stderr $ "Can not find exe-file:" ++ name
      exitWith $ ExitFailure 2
      
main :: IO ()
main = do
  args <- getArgs
  case args of
    (dir:binName:[]) -> do
      binPath <- searchBinary dir binName
      putStrLn binPath
      exitWith $ ExitSuccess
    _ -> do
      hPutStrLn stderr "Usage: cabal-test-bin \"project's root directory\" \"executable-program-name\""
      exitWith $ ExitFailure 1
