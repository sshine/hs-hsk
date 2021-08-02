{-# LANGUAGE TemplateHaskell #-}

module Language.Chinese.HSK where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Trie.Text (Trie)
import qualified Data.Trie.Text as Trie

import Data.FileEmbed

data HSKLevel
  = HSK1
  | HSK2
  | HSK3
  | HSK4
  | HSK5
  | HSK6
  | HSK7_9
  deriving (Eq, Ord, Show)

type WordList a = [(a, [Text])]

-- | Word lists divided into HSK levels
plecoWordlists :: WordList HSKLevel
plecoWordlists =
  [ (HSK1, fromWordlist $(embedFile "data/hsk30-1.txt"))
  , (HSK2, fromWordlist $(embedFile "data/hsk30-2.txt"))
  , (HSK3, fromWordlist $(embedFile "data/hsk30-3.txt"))
  , (HSK4, fromWordlist $(embedFile "data/hsk30-4.txt"))
  , (HSK5, fromWordlist $(embedFile "data/hsk30-5.txt"))
  , (HSK6, fromWordlist $(embedFile "data/hsk30-6.txt"))
  , (HSK7_9, fromWordlist $(embedFile "data/hsk30-7-9.txt"))
  ]
  where
    fromWordlist = Text.lines . Text.decodeUtf8

plecoTrie :: Trie HSKLevel
plecoTrie = Trie.fromList
  [ (word, level) | (level, wordList) <- plecoWordlists, word <- wordList ]

-- | @'longestPrefix' haystack trie@ is the longest prefix of @haystack@ that
-- exists in an HSK wordlist, along with its 'HSKLevel', or @Nothing@.
longestPrefix :: Text -> Trie HSKLevel -> Maybe (HSKLevel, Text, Int)
longestPrefix haystack trie = go 1
  where
    go :: Int -> Maybe (HSKLevel, Text, Int)
    go n = do
      guard (n <= Text.length haystack)
      let candidate = Text.take n haystack
      level <- Trie.lookup candidate trie
      go (n+1) <|> pure (level, candidate, n)

process :: Text -> Trie HSKLevel -> WordList HSKLevel
process "" _ = []
process haystack trie =
  case longestPrefix haystack trie of
    Nothing -> process (Text.drop 1 haystack) trie
    Just (level, needle, n) -> (level, [needle]) : process (Text.drop n haystack) trie
