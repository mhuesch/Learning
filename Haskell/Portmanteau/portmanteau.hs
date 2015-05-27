module Portmanteau where

import Data.Char (toLower)
import Data.List (tails,sortBy)
import Data.Set  (fromList,toList)
import Data.Maybe (catMaybes)
import Data.Functor
import Data.Function (on)
import Data.Foldable (forM_)
import System.Environment (getArgs)

englishWords :: IO [String]
englishWords = do
  ws <- lines <$> readFile "/usr/share/dict/words"
  return $ map (map toLower) ws

port :: String -> Int -> [String] -> [(String,String)]
port seed len ws = if len > seedLen
                      then error "len is too long!"
                      else catMaybes ps
  where
    seedLen = length seed
    (prefix,suffix) = splitAt (seedLen-len) seed
    matches w = if (take len w) == suffix
                   then Just $ (prefix ++ w, w)
                   else Nothing
    ps = map matches ws

portInfix :: String -> Int -> Int -> [String] -> [String]
portInfix seed nTails len ws = if len > seedLen
                           then error "len is too long!"
                           else catMaybes ps
  where
    seedLen = length seed
    (prefix,suffix) = splitAt (seedLen-len) seed
    matches w = if (take len w) == suffix
                   then Just $ prefix ++ w
                   else Nothing
    ps = map matches (concatMap (nTailsOfMinLen nTails len) ws)


nTailsOfMinLen :: Int -> Int -> [a] -> [[a]]
nTailsOfMinLen n len xs = take (n `min` (xsLen - len + 1)) (tails xs)
  where
    xsLen = length xs

dedup :: (Ord a) => [a] -> [a]
dedup = toList . fromList

-- chunks :: Int -> [a] -> [[(Int,a)]]
-- chunks len xs = if len > xsLen
--                    then []
--                    else map f [0..(xsLen-len)]
--   where
--     xsLen = length xs
--     f idx = (idx, take len . drop idx $ xs)


main = do
  [word,len,minPortAdded,nTails] <- getArgs
  prts <- portInfix word (read nTails) (read len) <$> englishWords
  let isMinLen xs = (length word + (read minPortAdded)) < length xs
      clean = sortBy (compare `on` length) . dedup . filter isMinLen
  mapM_ putStrLn (clean prts)
