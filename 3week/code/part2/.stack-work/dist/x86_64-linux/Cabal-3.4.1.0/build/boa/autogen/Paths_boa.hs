{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_boa (
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

bindir     = "/home/silthyflut42/AP/APGIT/AP/3week/code/part2/.stack-work/install/x86_64-linux/0ceb76ee2e67598bed1bc57bdaae0a93f32fb4666f89e078eaaa1db1f1abdfe9/9.0.2/bin"
libdir     = "/home/silthyflut42/AP/APGIT/AP/3week/code/part2/.stack-work/install/x86_64-linux/0ceb76ee2e67598bed1bc57bdaae0a93f32fb4666f89e078eaaa1db1f1abdfe9/9.0.2/lib/x86_64-linux-ghc-9.0.2/boa-0.0.0-9e6v4sw0aL0k6SQZmEDjv-boa"
dynlibdir  = "/home/silthyflut42/AP/APGIT/AP/3week/code/part2/.stack-work/install/x86_64-linux/0ceb76ee2e67598bed1bc57bdaae0a93f32fb4666f89e078eaaa1db1f1abdfe9/9.0.2/lib/x86_64-linux-ghc-9.0.2"
datadir    = "/home/silthyflut42/AP/APGIT/AP/3week/code/part2/.stack-work/install/x86_64-linux/0ceb76ee2e67598bed1bc57bdaae0a93f32fb4666f89e078eaaa1db1f1abdfe9/9.0.2/share/x86_64-linux-ghc-9.0.2/boa-0.0.0"
libexecdir = "/home/silthyflut42/AP/APGIT/AP/3week/code/part2/.stack-work/install/x86_64-linux/0ceb76ee2e67598bed1bc57bdaae0a93f32fb4666f89e078eaaa1db1f1abdfe9/9.0.2/libexec/x86_64-linux-ghc-9.0.2/boa-0.0.0"
sysconfdir = "/home/silthyflut42/AP/APGIT/AP/3week/code/part2/.stack-work/install/x86_64-linux/0ceb76ee2e67598bed1bc57bdaae0a93f32fb4666f89e078eaaa1db1f1abdfe9/9.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "boa_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "boa_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "boa_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "boa_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "boa_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "boa_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
