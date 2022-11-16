import Data.Maybe (fromJust)
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
        confHook = addExtraLibs
      }

buildBackend :: Args -> ConfigFlags -> IO HookedBuildInfo
buildBackend _ _ = do
  readProcess "make" ["--directory=backend"] "" >>= putStrLn
  return emptyHookedBuildInfo

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
                          { extraLibDirs = (dir ++ "/dist-newstyle/backend-libs") : extraLibDirs libraryBuildInfo
                          }
                    }
            }
      }
