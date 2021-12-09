{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_snake (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/celiaxiao/cse230/haskell-flappy-bird/.stack-work/install/x86_64-linux/58149d7fab4dbd330614d7443538dbecdd9d7fc91e1011a6518255aec7477499/8.10.4/bin"
libdir     = "/home/celiaxiao/cse230/haskell-flappy-bird/.stack-work/install/x86_64-linux/58149d7fab4dbd330614d7443538dbecdd9d7fc91e1011a6518255aec7477499/8.10.4/lib/x86_64-linux-ghc-8.10.4/snake-0.1.0.0-28lLm1UW86gAI5Kumecdi5-snake"
dynlibdir  = "/home/celiaxiao/cse230/haskell-flappy-bird/.stack-work/install/x86_64-linux/58149d7fab4dbd330614d7443538dbecdd9d7fc91e1011a6518255aec7477499/8.10.4/lib/x86_64-linux-ghc-8.10.4"
datadir    = "/home/celiaxiao/cse230/haskell-flappy-bird/.stack-work/install/x86_64-linux/58149d7fab4dbd330614d7443538dbecdd9d7fc91e1011a6518255aec7477499/8.10.4/share/x86_64-linux-ghc-8.10.4/snake-0.1.0.0"
libexecdir = "/home/celiaxiao/cse230/haskell-flappy-bird/.stack-work/install/x86_64-linux/58149d7fab4dbd330614d7443538dbecdd9d7fc91e1011a6518255aec7477499/8.10.4/libexec/x86_64-linux-ghc-8.10.4/snake-0.1.0.0"
sysconfdir = "/home/celiaxiao/cse230/haskell-flappy-bird/.stack-work/install/x86_64-linux/58149d7fab4dbd330614d7443538dbecdd9d7fc91e1011a6518255aec7477499/8.10.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "snake_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "snake_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "snake_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "snake_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "snake_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "snake_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
