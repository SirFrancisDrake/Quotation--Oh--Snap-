module Quotes
  ( Quote(Quote)
  , fromStrings
  , wrapQuotes
  )
  where

import Control.Monad (join)
import Data.Function (on)
import Data.List (intersperse)

data Quote = Quote { q_id :: String
                   , q_text :: String
                   , q_author :: String
                   , q_approved_by :: String
                   , q_approved_date :: String
                   }
          deriving ()

instance Show Quote where
    show (Quote a b c d e) = join $ intersperse "\n" [a,b,c,d,e]

instance Eq Quote where
    (==) = on (==) q_id

instance Ord Quote where
    (>) = on (>) q_id

fromStrings :: [String] -> Quote
fromStrings (a:b:c:d:e:[]) = Quote a b c d e

wrapQuotes :: [Quote] -> String
wrapQuotes [] = []
wrapQuotes ((Quote id t a _ _):xs) = 
    "<p>id " ++ id ++ "<p>текст: " ++ t ++ "<p>автор: " ++ a 
    ++ "<p>" ++ wrapQuotes xs


