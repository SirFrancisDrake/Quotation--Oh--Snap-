module DBInteract where

import Control.Applicative ((<$>))
import Control.Monad (join)
import Database.HDBC hiding (fromSql) -- Data.Convertible is terrific, but seriously?
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import qualified Database.Redis.Redis as R
import qualified Data.ByteString as B
import Data.List (intersperse)
import Data.Maybe (isJust, fromJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Quotes
import SqlQuotes

-- PostgreSQL login information
host     = "host="     ++ "localhost"
db       = "dbname="   ++ "bashorg"
login    = "user="     ++ "bash"
password = "password=" ++ "mrshudson" -- password shouldn't be kept like that

-- Redis login information
rHost = R.localhost
rPort = R.defaultPort

connectToPostgres = do
    connectPostgreSQL $ join $ intersperse " " [host, db, login, password]

fetchQuotes :: IO [Quote]
fetchQuotes = do
    connection <- connectToPostgres
    select <- prepare connection "SELECT * FROM quotes"
    execute select []
    quotes <- parseQuotes <$> fetchAllRows select
    disconnect connection
    return quotes

insertQuote :: Quote -> IO String -- String as in ``debug message''. FIXME
insertQuote q = do
    let query = safeQuoteAddQuery q
    if isJust query then do connection <- connectToPostgres
                            insert <- prepare connection (fromJust query)
                            execute insert []
                            commit connection
                            disconnect connection
                            return "Quote inserted, ok."
                    else return "Quote failed"

-- adding a quote to PostgreSQL base shouldn't be complicated
-- same applies to redis

fetchApproveList :: IO [Quote]
fetchApproveList = do
    redis <- R.connect rHost rPort
    keys <- R.zrevrange redis "keys" (0,999999) True >>= R.fromRMultiBulk'
    args <- mapM (\t -> R.lrange redis t (0,-1) :: IO (R.Reply String)) keys
    -- That last line doesn't interpret in GHCi due to ambiguous types of lrange
    -- type constraint does solve this problem in REPL, but here he just ignores it
    -- So, FIXME
    reargs <- sequence $ map R.fromRMultiBulk' args
    let quotes = map fromStrings reargs
    return quotes
    
parseQuotes :: [[SqlValue]] -> [Quote]    
parseQuotes = map mkQuote

mkQuote :: [SqlValue] -> Quote
mkQuote xs = fromStrings $ map parse xs

parse :: SqlValue -> String
parse (SqlInteger a) = show a
parse SqlNull = ""
parse (SqlByteString s) = T.unpack $ T.decodeUtf8 s
parse _ = error "For a moment there I thought we were in trouble"
