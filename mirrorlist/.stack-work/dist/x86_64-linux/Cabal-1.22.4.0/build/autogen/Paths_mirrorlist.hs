module Paths_mirrorlist (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/arian/Projects/capita/mirrorlist/.stack-work/install/x86_64-linux/lts-3.16/7.10.2/bin"
libdir     = "/home/arian/Projects/capita/mirrorlist/.stack-work/install/x86_64-linux/lts-3.16/7.10.2/lib/x86_64-linux-ghc-7.10.2/mirrorlist-0.1.0.0-9iWvGynZLAH3bsYMIIsoTs"
datadir    = "/home/arian/Projects/capita/mirrorlist/.stack-work/install/x86_64-linux/lts-3.16/7.10.2/share/x86_64-linux-ghc-7.10.2/mirrorlist-0.1.0.0"
libexecdir = "/home/arian/Projects/capita/mirrorlist/.stack-work/install/x86_64-linux/lts-3.16/7.10.2/libexec"
sysconfdir = "/home/arian/Projects/capita/mirrorlist/.stack-work/install/x86_64-linux/lts-3.16/7.10.2/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "mirrorlist_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "mirrorlist_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "mirrorlist_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "mirrorlist_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "mirrorlist_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
