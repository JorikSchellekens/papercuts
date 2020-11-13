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
import qualified Data.ByteString.Lazy.UTF8 as LB8
  ( toString
  )
import Data.ByteString.UTF8
  ( toString
  )
import System.Directory
  ( getCurrentDirectory
  , getDirectoryContents
  )
import System.IO
  ( writeFile
  )
import System.FilePath
  ( (</>)
  , takeDirectory
  )
import Text.BibTeX.Parse
  ( file
  , skippingLeadingSpace
  , entry
  )
import qualified Text.BibTeX.Format as BibFormat
  ( entry
  )
import Text.BibTeX.Entry
  ( T
  )
import Text.Parsec
  ( parse
  , Stream
  )
import Text.Parsec.String
  ( parseFromFile
  )
import Text.Parsec.Error
  ( ParseError
  )
import Data.Text
  (Text
  , unpack
  )
import Data.Semigroup ((<>))

main :: IO ()
main = do
  (opts :: Opts) <- execParser optsParser
  case optCommand opts of
    Search str -> search str 4 >>= putStr
    Info id -> getBibtex id >>= print
    Add id -> add id
    Open id -> getPDFFromSciHub id >>= print

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
    <> command "search" (info (Search <$> argument str (metavar "SEARCH")) (progDesc "Searches crossref.org for papers"))
    <> command "open" (info (Open <$> argument str (metavar "ID")) (progDesc "Opens the paper as a pdf"))
    <> command "info" (info (Info <$> argument str (metavar "ID")) (progDesc "Gets paper bibtex info from doi.org"))
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

instance Show Work where
  show (Work title doi workType) = unpack $ doi <> " (" <> workType <> ") " <> (head title) 
instance FromJSON Work where
  parseJSON = withObject "Work" $ \v ->
    Work <$> v .: "title"
         <*> v .: "DOI"
         <*> v .: "type"

newtype Result = Result { fromResult :: [Work] }

instance Show Result where
  show works = unlines (show <$> (fromResult works))

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

search searchTerm n = scrapeCrossRef searchTerm n >>= \x -> pure $ case x of
  Just x -> show x
  Nothing -> "Couldn't parse results from crossRef"

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

getPDFFromSciHub doi = (toString <$> getSciHubLink doi) >>= simpleHTTP

getBibtex doi = (\x -> case x of
  Right e -> e
  Left e -> error $ "Couldn't parse doi bibtex for " <> doi) <$>
  (parseBibEntry . LB8.toString <$>
  (simpleHTTPWithHeaders [("Accept", "application/x-bibtex")] $ "http://dx.doi.org/" <> doi))

bibFile :: String
bibFile = "papercuts.bib"
bibtexLocation' :: FilePath -> IO (FilePath)
bibtexLocation' currentDir = 
  if currentDir == "/" then
    error "Not a papercut repo"
  else do
    contents <- getDirectoryContents currentDir
    if elem bibFile contents then
      pure $ currentDir </> bibFile
    else
      bibtexLocation' $ takeDirectory currentDir

bibtexLocation :: IO (FilePath)
bibtexLocation = getCurrentDirectory >>= bibtexLocation'

init :: IO (FilePath)
init = do
  currentDir <- getCurrentDirectory
  writeFile (currentDir </> bibFile) ""
  return $ currentDir </> bibFile

parseBibFile :: FilePath -> IO (Either ParseError [T])
parseBibFile = parseFromFile $ skippingLeadingSpace file

parseBibEntry = parse entry "Bibentry Parser"

add :: String -> IO ()
add doi = do
  bibLoc <- bibtexLocation
  newEntry <- getBibtex doi
  parseResult <- parseBibFile bibLoc
  case parseResult of
    Right entries -> writeFile bibLoc $ unlines . (BibFormat.entry <$>) $ newEntry : entries
    Left e -> error "Couldn't parse bib file"

