module SqlQuotes
 ( quoteAddQuery
 , safeQuoteAddQuery
 )
 where

-- quoteAddQuery turns a Quote into an INSERT query
-- safeQuoteAddQuery does tha same, or goes mad if the quote is empty

import Control.Monad (join)
import Data.List (intersperse)
import Database.HDBC hiding (fromSql) -- see comment in Site.hs

import Quotes
import SqlUtils (screenQuotes)

sqlize :: String -> String
sqlize "" = ""
sqlize a = "'" ++ screenQuotes a  ++ "'"

quoteAddQuery :: Quote -> String
quoteAddQuery (Quote _ t a b _) =
    let mb var val = if var == "" then "" else val
        st = mb t "text"
        sa = mb a "author"
        sb = mb b "approved_by"
        sd = ""
        d = "" -- This is temporary FIXME
        listify a = join $ intersperse ", " $ filter (/= "") a
    in "INSERT INTO quotes (" ++ listify [st,sa,sb,sd] ++ 
            ") VALUES (" ++ (listify $ map sqlize [t, a, b, d]) ++ ");"

safeQuoteAddQuery :: Quote -> Maybe String
safeQuoteAddQuery q@(Quote _ t a b _) =
    if and [ t /= ""
           , a /= ""
           , b /= "" ] 
       then Just $ quoteAddQuery q
       else Nothing
