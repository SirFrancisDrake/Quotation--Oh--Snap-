module SQLUtils
  ( screenQuotes
  )
  where

import Data.List (foldl')

screenQuotes :: String -> String
screenQuotes = foldl' (\acc t ->
                            if t == '\'' then acc ++ "\'\'" 
                                         else acc ++ [t])
                      "" -- acc ++ [t] should look like snoc acc t
                         -- but it didn't feel that intuitive to read
