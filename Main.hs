{-# LANGUAGE ScopedTypeVariables #-}

import qualified Control.Exception as E
import System.Environment
import System.Exit
import System.IO
import Test.Cabal.Path

main :: IO ()
main = do
  args <- getArgs
  case args of
    (dir:binName:[]) -> do
      eBinPath <- E.try $ getExePath dir binName
      case eBinPath of
        Right binPath -> do
          putStrLn binPath
          exitWith $ ExitSuccess
        Left (err::E.SomeException) -> do
          putStr $ show err
          exitWith $ ExitFailure 1
    _ -> do
      hPutStrLn stderr "Usage: cabal-test-bin \"project's root directory\" \"executable-program-name\""
      exitWith $ ExitFailure 1
