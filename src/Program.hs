{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

-- Example derived from:
--
-- https://hackage.haskell.org/package/optparse-generic-1.4.3/docs/Options-Generic.html

module Program where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Version (showVersion)
import Options.Generic

import Paths_hsk (version)
import qualified Language.Chinese.HSK as HSK

data Config w = Config
  { freq :: w ::: Bool <?> "Display the frequency of each HSK level"
  } deriving (Generic)

instance ParseRecord (Config Wrapped) where
  parseRecord = parseRecordWithModifiers modifiers

modifiers :: Modifiers
modifiers = defaultModifiers
  { shortNameModifier = firstLetter
  }

deriving instance Show (Config Unwrapped)

program :: IO ()
program = do
  config <- getConfig
  input <- Text.getContents
  print (HSK.process input HSK.plecoTrie)

getConfig :: IO (Config Unwrapped)
getConfig = unwrapRecord ("hs-hsk " <> versionText)

versionText :: Text
versionText = Text.pack (showVersion version)

