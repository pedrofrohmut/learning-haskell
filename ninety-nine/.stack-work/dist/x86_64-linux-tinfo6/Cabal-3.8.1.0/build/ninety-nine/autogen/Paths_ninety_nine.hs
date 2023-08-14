{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_ninety_nine (
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
bindir     = "/home/pedro/programming/learning/learning-haskell/ninety-nine/.stack-work/install/x86_64-linux-tinfo6/1023601b36c2e205d90e7e42942803bda1df7e7c6f7d692e68b11e60936d89fc/9.4.5/bin"
libdir     = "/home/pedro/programming/learning/learning-haskell/ninety-nine/.stack-work/install/x86_64-linux-tinfo6/1023601b36c2e205d90e7e42942803bda1df7e7c6f7d692e68b11e60936d89fc/9.4.5/lib/x86_64-linux-ghc-9.4.5/ninety-nine-0.1.0.0-6JiLl25S0wr8iM0UDuJEgu-ninety-nine"
dynlibdir  = "/home/pedro/programming/learning/learning-haskell/ninety-nine/.stack-work/install/x86_64-linux-tinfo6/1023601b36c2e205d90e7e42942803bda1df7e7c6f7d692e68b11e60936d89fc/9.4.5/lib/x86_64-linux-ghc-9.4.5"
datadir    = "/home/pedro/programming/learning/learning-haskell/ninety-nine/.stack-work/install/x86_64-linux-tinfo6/1023601b36c2e205d90e7e42942803bda1df7e7c6f7d692e68b11e60936d89fc/9.4.5/share/x86_64-linux-ghc-9.4.5/ninety-nine-0.1.0.0"
libexecdir = "/home/pedro/programming/learning/learning-haskell/ninety-nine/.stack-work/install/x86_64-linux-tinfo6/1023601b36c2e205d90e7e42942803bda1df7e7c6f7d692e68b11e60936d89fc/9.4.5/libexec/x86_64-linux-ghc-9.4.5/ninety-nine-0.1.0.0"
sysconfdir = "/home/pedro/programming/learning/learning-haskell/ninety-nine/.stack-work/install/x86_64-linux-tinfo6/1023601b36c2e205d90e7e42942803bda1df7e7c6f7d692e68b11e60936d89fc/9.4.5/etc"

getBinDir     = catchIO (getEnv "ninety_nine_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "ninety_nine_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "ninety_nine_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "ninety_nine_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ninety_nine_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ninety_nine_sysconfdir") (\_ -> return sysconfdir)




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
