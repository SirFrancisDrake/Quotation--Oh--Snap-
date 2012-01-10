module Paths_snap_project (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/sir-francis/.cabal/bin"
libdir     = "/home/sir-francis/.cabal/lib/snap-project-0.1/ghc-7.0.3"
datadir    = "/home/sir-francis/.cabal/share/snap-project-0.1"
libexecdir = "/home/sir-francis/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "snap_project_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "snap_project_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "snap_project_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "snap_project_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
