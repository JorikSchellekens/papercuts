{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}


module Main where

import Options.Applicative
  ( ParserInfo
  , info
  , argument
  , execParser
  , str
  , command
  , subparser
  , metavar
  , progDesc
  )
import Data.Aeson
  ( eitherDecode
  , encode
  , FromJSON
  , (.:)
  , parseJSON
  , withObject
  , fromJSON
  , withArray
  , decode
  )
import Network.HTTP.Simple
  ( httpLBS
  , parseRequest_
  , setRequestHeaders
  , getResponseBody
  )
import Network.HTTP.Types.Header
  ( HeaderName
  )
import Text.Regex.TDFA
  ( (=~)
  )
import Data.ByteString.Lazy.Char8
  ( toStrict
  )
import Data.ByteString
  ( ByteString
  )
import Data.Text (Text)
import Data.Semigroup ((<>))


main :: IO ()
main = do
  (opts :: Opts) <- execParser optsParser
  case optCommand opts of
    Search id -> scrapeCrossRef id 4 >>= print
  print $ optCommand opts

data Opts = Opts
  { optCommand :: !Command
  }

type Id = String

data Command
  = Init
  | Search Id
  | Open Id
  | Info Id
  | Add Id
  | List
  | Remove Id
  | Sync
  deriving (Show)

optsParser :: ParserInfo Opts
optsParser = info (programOptions) (progDesc "Description")

programOptions = Opts <$> opts

opts = subparser
  (
    command "init" (info (pure Init) (progDesc "Initialize a papers index"))
    <> command "search" (info (Search <$> argument str (metavar "SEARCH")) (progDesc "Searches google scholar for papers"))
    <> command "open" (info (Open <$> argument str (metavar "ID")) (progDesc "Opens the paper as a pdf"))
    <> command "info" (info (Info <$> argument str (metavar "ID")) (progDesc "Gets paper info from google scholar"))
    <> command "add" (info (Add <$> argument str (metavar "ID")) (progDesc "Adds the paper to the index"))
    <> command "list" (info (pure List) (progDesc "Lists all paper id's and titles"))
    <> command "remove" (info (Remove <$> argument str (metavar "ID")) $ progDesc "Removes a paper from the index")
    <> command "sync" (info (pure Sync) (progDesc "Download all papers"))
  )

data Work =
  Work { titles :: [Text]
       , doi :: Text
       , workType :: Text
       }
  deriving (Show)

instance FromJSON Work where
  parseJSON = withObject "Work" $ \v ->
    Work <$> v .: "title"
         <*> v .: "DOI"
         <*> v .: "type"

newtype Result = Result { fromResult :: [Work] }
  deriving (Show)

instance FromJSON Result where
  parseJSON = withObject "RESULT" $ \v ->
    (v .: "message") >>=
    (.: "items") >>=
    (\a -> Result <$> (parseJSON a))

simpleHTTP url = getResponseBody <$> ((httpLBS . parseRequest_) $ url)

simpleHTTPWithHeaders headers url = getResponseBody <$>
  ((httpLBS . setRequestHeaders headers . parseRequest_) $ url)

scrapeCrossRef :: String -> Int -> IO (Maybe Result)
scrapeCrossRef searchTerm n = decode <$>
  (simpleHTTP $
    "https://api.crossref.org/works?query=" <> searchTerm <>
    "&rows=" <> (show n)
  )

downloadRegex = "(\\/\\/.*)\\?download"
getSciHubLink' :: String -> IO (ByteString, ByteString, ByteString, [ByteString])
getSciHubLink' doi = do
  sciHubResult <- simpleHTTP $ "https://sci-hub.do/" <> doi
  return $ ((=~ (downloadRegex :: ByteString)) . toStrict) sciHubResult

getSciHubLink :: String -> IO ByteString
getSciHubLink doi = do 
  tuple <- getSciHubLink' doi
  let (_,_,_,[res]) = tuple in
    return $ "https:" <> res

getBibtex doi = simpleHTTPWithHeaders [("Accept", "application/x-bibtex")] $ "http://dx.doi.org/" <> doi
