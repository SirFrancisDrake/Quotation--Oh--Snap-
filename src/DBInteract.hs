module DBInteract where

import Control.Applicative ((<$>))
import Control.Monad (join)
import Database.HDBC hiding (fromSql) -- Data.Convertible is terrific, but seriously?
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import qualified Database.Redis.Redis as R
import qualified Data.ByteString as B
import Data.List (intersperse)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Quotes

-- PostgreSQL login information
host     = "host="     ++ "localhost"
db       = "dbname="   ++ "bashorg"
login    = "user="     ++ "bash"
password = "password=" ++ "mrshudson" -- password shouldn't be kept like that

-- Redis login information
rHost = R.localhost
rPort = R.defaultPort

fetchQuotes :: IO [Quote]
fetchQuotes = do
    connection <- connectPostgreSQL $ join $ intersperse " " [host, db, login, password]
    select <- prepare connection "SELECT * FROM quotes"
    execute select []
    parseQuotes <$> fetchAllRows select

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
