import Data.Maybe (fromJust)
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import System.Process
import System.Directory

redCol = "\ESC[31m"
resetCol = "\ESC[0m"

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
  putStrLn $ redCol ++ ">> Running Pre-Configure Hook  <<" ++ resetCol
  readProcess "make" ["--directory=backend"] "" >>= putStrLn
  putStrLn $ redCol ++ ">> Pre-Configure Hook Complete <<" ++ resetCol
  return emptyHookedBuildInfo

buildBackendPreBuild :: Args -> BuildFlags -> IO HookedBuildInfo
buildBackendPreBuild _ _ = do
  putStrLn $ redCol ++ ">> Running Pre-Build Hook  <<" ++ resetCol
  readProcess "make" ["--directory=backend"] "" >>= putStrLn
  putStrLn $ redCol ++ ">> Pre-Build Hook Complete <<" ++ resetCol
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
                          { extraLibDirs = ("/usr/local/lib") : (dir ++ "/backend/lib") : extraLibDirs libraryBuildInfo
                          }
                    }
            }
      }
