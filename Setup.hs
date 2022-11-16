module Main (main) where

import Control.Monad
  ( unless,
    when,
  )
import Data.Maybe
  ( fromJust,
    fromMaybe,
  )
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import System.Process
import System.Directory

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { preConf = buildBackend,
        confHook = addExtraLibs,
        postClean = cmakeClean
      }

buildBackend :: Args -> ConfigFlags -> IO HookedBuildInfo
buildBackend _ _ = do
  readProcess "make" ["--directory=backend"] "" >>= putStrLn
  return emptyHookedBuildInfo

{- See https://github.com/haskell/cabal/issues/6112
`cabal clean/new-clean/v2-clean does not consider the extra tmp files, and 
hence no hooks are run, this is a bug with cabal. -}
cmakeClean :: Args -> CleanFlags -> PackageDescription -> () -> IO ()
cmakeClean _ _ _ _ =
    readProcess "make" ["clean", "--directory=backend"] "" >>= putStrLn

addExtraLibs ::
  (GenericPackageDescription, HookedBuildInfo) ->
  ConfigFlags ->
  IO LocalBuildInfo
addExtraLibs (description, buildInfo) flags = do
  localBuildInfo <- confHook simpleUserHooks (description, buildInfo) flags
  let packageDescription = localPkgDescr localBuildInfo
      lib = fromJust $ library packageDescription
      libraryBuildInfo   = libBuildInfo lib
  dir <- getCurrentDirectory
  return
    localBuildInfo
      { localPkgDescr =
          packageDescription
            { library =
                Just $ lib
                    { libBuildInfo =
                        libraryBuildInfo
                          { extraLibDirs = (dir ++ "/backend/libs") : extraLibDirs libraryBuildInfo
                          }
                    }
            }
      }
