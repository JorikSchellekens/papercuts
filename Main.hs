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
import Text.HTML.Scalpel 
  ( chroots
  , scrapeURL
  , (@:)
  , text
  , hasClass
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
  )
import Data.Text (Text)
import Data.Semigroup ((<>))


main :: IO ()
main = do
    (opts :: Opts) <- execParser optsParser
    case optCommand opts of
      Search id -> scrapeScholars id >>= print
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
  )

data Work =
  Work { titles :: ![Text]
       , doi :: !Text
       , workType :: !Text
       }
  deriving (Show)

instance FromJSON Work where
  parseJSON = withObject "Work" $ \v ->
    Work <$> v .: "title"
         <*> v .: "DOI"
         <*> v .: "type"

instance FromJSON ([Work]) where
  parseJSON = withObject "Works" $ \v ->
    withArray "itemarray" (\a -> do
      return fromJSON a :: Work) (v .: "items")
    
scrapeScholars :: String -> IO (Maybe [String])
scrapeScholars searchTerm = scrapeURL ("https://scholar.google.com/scholar?q=" <> searchTerm) papers
  where
    papers = chroots ("div" @: [hasClass ".gs_r"]) title
    title = do
      t <- text $ "h3"
      return t
