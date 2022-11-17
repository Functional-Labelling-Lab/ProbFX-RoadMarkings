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
      { preConf = buildBackendPreConf,
        confHook = addExtraLibs,
        preBuild = buildBackendPreBuild
      }

buildBackendPreConf :: Args -> ConfigFlags -> IO HookedBuildInfo
buildBackendPreConf _ _ = do
  putStrLn "\n>> Running Pre-Configure Hook  <<\n"
  readProcess "make" ["--directory=backend"] "" >>= putStrLn
  putStrLn ">> Pre-Configure Hook Complete <<\n"
  return emptyHookedBuildInfo

buildBackendPreBuild :: Args -> BuildFlags -> IO HookedBuildInfo
buildBackendPreBuild _ _ = do
  putStrLn "\n>> Running Pre-Build Hook  <<\n"
  readProcess "make" ["--directory=backend"] "" >>= putStrLn
  putStrLn ">> Pre-Build Hook Complete <<\n"
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
                          { extraLibDirs = ("/usr/local/lib") : (dir ++ "/dist-newstyle/backend-libs") : extraLibDirs libraryBuildInfo
                          }
                    }
            }
      }
