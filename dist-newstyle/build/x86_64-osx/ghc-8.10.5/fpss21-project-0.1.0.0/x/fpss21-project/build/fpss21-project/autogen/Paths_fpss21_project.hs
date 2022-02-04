{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_fpss21_project (
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

bindir     = "/Users/gbukov/.cabal/bin"
libdir     = "/Users/gbukov/.cabal/lib/x86_64-osx-ghc-8.10.5/fpss21-project-0.1.0.0-inplace-fpss21-project"
dynlibdir  = "/Users/gbukov/.cabal/lib/x86_64-osx-ghc-8.10.5"
datadir    = "/Users/gbukov/.cabal/share/x86_64-osx-ghc-8.10.5/fpss21-project-0.1.0.0"
libexecdir = "/Users/gbukov/.cabal/libexec/x86_64-osx-ghc-8.10.5/fpss21-project-0.1.0.0"
sysconfdir = "/Users/gbukov/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "fpss21_project_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "fpss21_project_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "fpss21_project_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "fpss21_project_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "fpss21_project_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "fpss21_project_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
