{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_defiende_la_torre (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/mauricio/Documentos/septimo semestre/Programacion_Funcional/Defiende la torre/.stack-work/install/x86_64-linux-tinfo6/027eb0bada907b5dd8e5f0f3ffeef3cce1dc8100bbd96c778d8df9f4560b3aee/9.2.7/bin"
libdir     = "/home/mauricio/Documentos/septimo semestre/Programacion_Funcional/Defiende la torre/.stack-work/install/x86_64-linux-tinfo6/027eb0bada907b5dd8e5f0f3ffeef3cce1dc8100bbd96c778d8df9f4560b3aee/9.2.7/lib/x86_64-linux-ghc-9.2.7/defiende-la-torre-0.1.0.0-1BAo0ikNc14FYzdaRECp4D-defiende"
dynlibdir  = "/home/mauricio/Documentos/septimo semestre/Programacion_Funcional/Defiende la torre/.stack-work/install/x86_64-linux-tinfo6/027eb0bada907b5dd8e5f0f3ffeef3cce1dc8100bbd96c778d8df9f4560b3aee/9.2.7/lib/x86_64-linux-ghc-9.2.7"
datadir    = "/home/mauricio/Documentos/septimo semestre/Programacion_Funcional/Defiende la torre/.stack-work/install/x86_64-linux-tinfo6/027eb0bada907b5dd8e5f0f3ffeef3cce1dc8100bbd96c778d8df9f4560b3aee/9.2.7/share/x86_64-linux-ghc-9.2.7/defiende-la-torre-0.1.0.0"
libexecdir = "/home/mauricio/Documentos/septimo semestre/Programacion_Funcional/Defiende la torre/.stack-work/install/x86_64-linux-tinfo6/027eb0bada907b5dd8e5f0f3ffeef3cce1dc8100bbd96c778d8df9f4560b3aee/9.2.7/libexec/x86_64-linux-ghc-9.2.7/defiende-la-torre-0.1.0.0"
sysconfdir = "/home/mauricio/Documentos/septimo semestre/Programacion_Funcional/Defiende la torre/.stack-work/install/x86_64-linux-tinfo6/027eb0bada907b5dd8e5f0f3ffeef3cce1dc8100bbd96c778d8df9f4560b3aee/9.2.7/etc"

getBinDir     = catchIO (getEnv "defiende_la_torre_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "defiende_la_torre_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "defiende_la_torre_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "defiende_la_torre_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "defiende_la_torre_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "defiende_la_torre_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
