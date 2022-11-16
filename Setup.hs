module Main (main) where

import Control.Monad
    ( unless
    , when
    )
import Data.Maybe
    ( fromJust
    , fromMaybe
    )
import qualified Distribution.PackageDescription as PD
import Distribution.Simple
    ( Args
    , UserHooks
    , buildHook
    , confHook
    , defaultMainWithHooks
    , postClean
    , postConf
    , preConf
    , simpleUserHooks
    )
import Distribution.Simple.LocalBuildInfo
    ( LocalBuildInfo
    , configFlags
    , localPkgDescr
    )
import Distribution.Simple.Setup
    ( BuildFlags
    , CleanFlags
    , ConfigFlags
    , buildVerbosity
    , cleanVerbosity
    , configConfigurationsFlags
    , configVerbosity
    , fromFlag
    )
import Distribution.Simple.Utils (rawSystemExit)
import System.Directory
    ( doesDirectoryExist
    , getCurrentDirectory
    , removeDirectoryRecursive
    )

main :: IO ()
main = defaultMainWithHooks simpleUserHooks {
           confHook = roadMarkingsConfHook
       }

roadMarkingsConfHook :: (PD.GenericPackageDescription, PD.HookedBuildInfo) ->
                  ConfigFlags ->
                  IO LocalBuildInfo
roadMarkingsConfHook (description, buildInfo) flags = do
    localBuildInfo <- confHook simpleUserHooks (description, buildInfo) flags
    let packageDescription = localPkgDescr localBuildInfo
        library = fromJust $ PD.library packageDescription
        libraryBuildInfo = PD.libBuildInfo library
    dir <- getCurrentDirectory
    return localBuildInfo {
        localPkgDescr = packageDescription {
            PD.library = Just $ library {
                PD.libBuildInfo = libraryBuildInfo {
                    PD.includeDirs = (dir ++ "/backend/build"):PD.includeDirs libraryBuildInfo,
                    PD.extraLibDirs = (dir ++ "/backend/build"):PD.extraLibDirs libraryBuildInfo
                }
            }
        }
    }