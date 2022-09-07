{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module CustomSearchEngine where

import System.IO

import Data.Function

import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Exception (throwIO)
import Control.Applicative
import Control.Monad
--------------------------------------------
import Text.HTML.Scalpel

import Network.HTTP.Client (HttpException)

import Cob.RecordM.TH
import Cob.RecordM.Query
import Cob.Ref
import Cob

data IndexEntry = IndexEntry { _user    :: String
                             , _url     :: String
                             , _title   :: Maybe Text
                             , _description :: Maybe Text
                             , _headings :: Maybe Text
                             , _content :: Maybe Text
                             }

mkRecord ''IndexEntry "Index Entries" ["User", "URL", "Title", "Description", "Headings", "Content"]

-- | Get links according to query
searchFor :: String -> Cob [IndexEntry]
searchFor q = map snd <$> search ((fromString q) { _size = 30 })

-- | Adds a link to your index
addLink :: URL -> Cob ()
addLink url = (`catch` (\(e :: HttpException) -> liftCob $ print e)) $ do
  Just index <- liftCob $ scrapeURL url (scraper url "romes")
  _ <- add index
  liftCob $ putStrLn ("Added: " <> url)

  where
    scraper :: String -> String -> Scraper Text IndexEntry
    scraper url user = do
      title <- Just <$> (text "title") <|> pure Nothing
      desc  <- Just <$> attr "content" ("meta" @: ["name" @= "description"]) <|> pure Nothing
      h1 <- texts "h1"
      h2 <- texts "h2"
      h3 <- texts "h3"
      h4 <- texts "h4"
      content <- Just . T.unlines <$> texts "p"
      let headings = case h1 <> h2 <> h3 <> h4 of
                       [] -> Nothing
                       hs -> Just $ T.unwords hs
      pure (IndexEntry user url title desc headings content)


-- | Import from firefox exported bookmarks as html
importFromFirefoxExport :: FilePath -> Cob ()
importFromFirefoxExport file = do

  cont <- liftCob (T.readFile file)

  case scrapeStringLike cont scrpr of

    Nothing -> liftCob (fail "Couldn't parse file")

    Just links -> do
      -- todo: Don't ignore titles and tags...

      mapConcurrently (addLink . T.unpack . fst) links
      pure ()

  where
    scrpr :: Scraper Text [(Text, Text)] -- ^ (URL, Title)
    -- scrape bookmark folders (triply nested dl)
    scrpr = chroot ("dl" // "dl" // "dl") $ do
      hrefs  <- attrs "href" "a"
      titles <- texts "a"
      pure (zip hrefs titles)

  -- way too exagerated for this
    -- File.toBytes file
  -- & U.decodeUtf8
  -- & U.lines Fold.toList   -- Serial String (line)
  -- & S.mapM  (putStrLn)
  -- & S.drain


instance Show IndexEntry where
  show (IndexEntry _ u t d _ _) = (<> u <> "\n\n") $ T.unpack $
    (case t of
      Just t' -> t' <> "\n"
      Nothing -> "") <>
    (case d of
       Just d' -> "  " <> d' <> "\n"
       Nothing -> "")
    
