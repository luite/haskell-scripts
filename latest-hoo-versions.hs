#!/usr/bin/env runhaskell

{-
  find all package-version.hoo files in a directory
  and print a list of the latest version of each package
-}

import Control.Applicative
import System.Environment
import System.Directory
import Data.List (isSuffixOf, intercalate)
import Data.List.Split
import qualified Data.Map as M

main = do
  args <- getArgs
  case args of
    [dir] -> latestHooVersions dir
    _     -> putStrLn "usage: latest-hoo-versions.hs dir"

latestHooVersions dir = do
  hoos <- filter (".hoo" `isSuffixOf`) <$> getDirectoryContents dir
  let hoos' = map parseHoo hoos
  let highestHoos  = M.toList . M.fromListWith max $ hoos'
  putStrLn . intercalate " " . map showPkg $ highestHoos
    where
      showPkg (name, ver) =
        dir ++ "/" ++ name ++ "-" ++ (intercalate "." . map show $ ver) ++ ".hoo"

parseHoo :: String -> (String, [Integer])
parseHoo file = (name, version)
  where
    pkgNameVer = (reverse . drop 4 . reverse) file
    pkgSplit   = splitOn "-" pkgNameVer
    name       = (intercalate "-" . init) pkgSplit
    version    = (map read . splitOn "." . last) pkgSplit