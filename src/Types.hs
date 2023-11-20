{-# LANGUAGE DeriveDataTypeable #-}
module Types
  ( Continue(..)
  , ReloadMode(..)
  , TermSize(..)
  -- Options and its deps
  , ColorMode(..)
  , Options(..)
  ) where

import Data.Data (Data)
import Data.Typeable (Typeable)
import Language.Haskell.Ghcid.Escape (WordWrap)
import System.Time.Extra (Seconds)

data Continue = Continue

data ReloadMode = Reload | Restart deriving (Show, Ord, Eq)

data TermSize = TermSize
    {termWidth :: Int
    ,termHeight :: Maybe Int -- ^ Nothing means the height is unlimited
    ,termWrap :: WordWrap
    }

-- | When to colour terminal output.
data ColorMode
    = Never  -- ^ Terminal output will never be coloured.
    | Always -- ^ Terminal output will always be coloured.
    | Auto   -- ^ Terminal output will be coloured if $TERM and stdout appear to support it.
      deriving (Show, Typeable, Data)

-- | Command line options
data Options = Options
    {command :: String
    ,arguments :: [String]
    ,test :: [String]
    ,test_message :: String
    ,run :: [String]
    ,warnings :: Bool
    ,lint :: Maybe String
    ,no_status :: Bool
    ,clear :: Bool
    ,reverse_errors :: Bool
    ,no_height_limit :: Bool
    ,height :: Maybe Int
    ,width :: Maybe Int
    ,topmost :: Bool
    ,no_title :: Bool
    ,project :: String
    ,reload :: [FilePath]
    ,restart :: [FilePath]
    ,directory :: FilePath
    ,outputfile :: [FilePath]
    ,ignoreLoaded :: Bool
    ,poll :: Maybe Seconds
    ,max_messages :: Maybe Int
    ,color :: ColorMode
    ,setup :: [String]
    ,allow_eval :: Bool
    ,target :: [String]
    }
    deriving (Data,Typeable,Show)


