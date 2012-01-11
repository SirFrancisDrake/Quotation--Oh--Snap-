module SqlQuotes
 ( empty
 , quoteAddQuery
 )
 where

import Control.Monad (join)
import Data.List (intersperse)
import Database.HDBC hiding (fromSql) -- see comment in Site.hs

import Quotes
import SQLUtils (screenQuotes)

data SqlQuote = SqlQuote { sq_id :: SqlValue
                         , sq_text :: SqlValue
                         , sq_author :: SqlValue
                         , sq_approved_by :: SqlValue
                         , sq_approved_date :: SqlValue
                         }
          deriving (Show)

empty :: SqlQuote -> Bool
empty (SqlQuote a b c d e) =
 and [ a == SqlNull
     , b == SqlNull
     , c == SqlNull
     , d == SqlNull
     , e == SqlNull ]
 
stringToSql :: String -> SqlValue
stringToSql s = if s == "" then SqlNull
                           else toSql $ screenQuotes s

fromQuote :: Quote -> SqlQuote
fromQuote (Quote _ t au ab _) = 
    let id = SqlNull
        text = stringToSql t
        author = stringToSql au
        approved_by = toSql ab
        approved_date = SqlNull
    in SqlQuote id text author approved_by approved_date

fromSql :: SqlValue -> String
fromSql (SqlString a) = "'" ++ a ++ "'"
fromSql SqlNull = ""

quoteAddQuery :: SqlQuote -> String
quoteAddQuery (SqlQuote _ t a b _) =
    let mb var val = if var == SqlNull then "" else val
        st = mb t "text"
        sa = mb a "author"
        sb = mb b "approved_by"
        sd = ""
        d = SqlNull -- This is temporary FIXME
        listify a = join $ intersperse ", " $ filter (/= "") a
    in "INSERT INTO quotes (" ++ listify [st,sa,sb,sd] ++ 
            ") VALUES (" ++ (listify $ map fromSql [t, a, b, d]) ++ ");"

safeQuoteAddQuery :: SqlQuote -> Maybe String
safeQuoteAddQuery q@(SqlQuote _ t a b _) =
    if and [ fromSql t /= ""
           , fromSql a /= ""
           , fromSql b /= ""] 
       then Just $ quoteAddQuery q
       else Nothing
