{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_ProbFX_RoadMarkings (
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

bindir     = "/home/barcisz/Downloads/ProbFX-RoadMarkings/.stack-work/install/x86_64-linux-tinfo6/31951932e13bee28f16cc7e772dbc5caa966da48287a9efbbe4204c830af24ef/9.0.2/bin"
libdir     = "/home/barcisz/Downloads/ProbFX-RoadMarkings/.stack-work/install/x86_64-linux-tinfo6/31951932e13bee28f16cc7e772dbc5caa966da48287a9efbbe4204c830af24ef/9.0.2/lib/x86_64-linux-ghc-9.0.2/ProbFX-RoadMarkings-0.1.0.0-5fIKr7pJqdBLCfLXnjdPg1-ProbFX-RoadMarkings-exe"
dynlibdir  = "/home/barcisz/Downloads/ProbFX-RoadMarkings/.stack-work/install/x86_64-linux-tinfo6/31951932e13bee28f16cc7e772dbc5caa966da48287a9efbbe4204c830af24ef/9.0.2/lib/x86_64-linux-ghc-9.0.2"
datadir    = "/home/barcisz/Downloads/ProbFX-RoadMarkings/.stack-work/install/x86_64-linux-tinfo6/31951932e13bee28f16cc7e772dbc5caa966da48287a9efbbe4204c830af24ef/9.0.2/share/x86_64-linux-ghc-9.0.2/ProbFX-RoadMarkings-0.1.0.0"
libexecdir = "/home/barcisz/Downloads/ProbFX-RoadMarkings/.stack-work/install/x86_64-linux-tinfo6/31951932e13bee28f16cc7e772dbc5caa966da48287a9efbbe4204c830af24ef/9.0.2/libexec/x86_64-linux-ghc-9.0.2/ProbFX-RoadMarkings-0.1.0.0"
sysconfdir = "/home/barcisz/Downloads/ProbFX-RoadMarkings/.stack-work/install/x86_64-linux-tinfo6/31951932e13bee28f16cc7e772dbc5caa966da48287a9efbbe4204c830af24ef/9.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ProbFX_RoadMarkings_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ProbFX_RoadMarkings_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "ProbFX_RoadMarkings_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "ProbFX_RoadMarkings_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ProbFX_RoadMarkings_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ProbFX_RoadMarkings_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
