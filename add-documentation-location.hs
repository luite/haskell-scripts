#!/usr/bin/env runhaskell

-- used by update-hoogle, used to add the correct documentation urls
-- in the hoogle database

import Control.Applicative
import System.Directory
import System.Environment
import System.IO
import Data.List as L

docPath = (++ "/.cabal/share/doc") <$> getHomeDirectory

getUrl pkg mod = do
  doc <- docPath
  return $ "file://" ++ doc ++ "/" ++ pkg ++ "/html/" ++ mod' ++ ".html"
  where
    mod' = map (\x -> if x == '.' then '-' else x) mod

main = do
  args <- getArgs
  case args of
    [pkg, file] -> transform pkg file
    _           -> hPutStrLn stderr "Usage: add-documentation-location.hs packagename filename"

transform pkg file = do
  ls <- lines <$> readFile file
  mapM_ (transformLine pkg) ls

transformLine pkg l
  | "module " `L.isPrefixOf` l = do
     url <- getUrl pkg (drop 7 l)
     putStrLn $ concat ["@url ", url, "\n", l]
  | otherwise                  = putStrLn l

