{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_warmup (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/tomato/git/AP/week1/code/part1/.stack-work/install/aarch64-osx/7ee89f23c2a0e68f5af5d37a5d8687d104bd9779c0697be8cee4dc504066bf06/9.0.2/bin"
libdir     = "/Users/tomato/git/AP/week1/code/part1/.stack-work/install/aarch64-osx/7ee89f23c2a0e68f5af5d37a5d8687d104bd9779c0697be8cee4dc504066bf06/9.0.2/lib/aarch64-osx-ghc-9.0.2/warmup-0.0.0-2FbuaVq49FT7Shs8CIRJJm"
dynlibdir  = "/Users/tomato/git/AP/week1/code/part1/.stack-work/install/aarch64-osx/7ee89f23c2a0e68f5af5d37a5d8687d104bd9779c0697be8cee4dc504066bf06/9.0.2/lib/aarch64-osx-ghc-9.0.2"
datadir    = "/Users/tomato/git/AP/week1/code/part1/.stack-work/install/aarch64-osx/7ee89f23c2a0e68f5af5d37a5d8687d104bd9779c0697be8cee4dc504066bf06/9.0.2/share/aarch64-osx-ghc-9.0.2/warmup-0.0.0"
libexecdir = "/Users/tomato/git/AP/week1/code/part1/.stack-work/install/aarch64-osx/7ee89f23c2a0e68f5af5d37a5d8687d104bd9779c0697be8cee4dc504066bf06/9.0.2/libexec/aarch64-osx-ghc-9.0.2/warmup-0.0.0"
sysconfdir = "/Users/tomato/git/AP/week1/code/part1/.stack-work/install/aarch64-osx/7ee89f23c2a0e68f5af5d37a5d8687d104bd9779c0697be8cee4dc504066bf06/9.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "warmup_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "warmup_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "warmup_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "warmup_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "warmup_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "warmup_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
