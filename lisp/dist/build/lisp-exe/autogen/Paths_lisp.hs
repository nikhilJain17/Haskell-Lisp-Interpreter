{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_lisp (
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

bindir     = "/Users/nikhilhw/.cabal/bin"
libdir     = "/Users/nikhilhw/.cabal/lib/x86_64-osx-ghc-8.6.5/lisp-0.1.0.0-CPmZr6HPG9Q5745w7sb1Yn-lisp-exe"
dynlibdir  = "/Users/nikhilhw/.cabal/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/nikhilhw/.cabal/share/x86_64-osx-ghc-8.6.5/lisp-0.1.0.0"
libexecdir = "/Users/nikhilhw/.cabal/libexec/x86_64-osx-ghc-8.6.5/lisp-0.1.0.0"
sysconfdir = "/Users/nikhilhw/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "lisp_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "lisp_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "lisp_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "lisp_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "lisp_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "lisp_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
