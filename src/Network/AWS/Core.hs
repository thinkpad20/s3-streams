{-# LANGUAGE OverloadedStrings,
             RecordWildCards,
             LambdaCase,
             ViewPatterns,
             ScopedTypeVariables,
             NoMonomorphismRestriction #-}
module Network.AWS.Core (
    module Codec.Utils
  , module Control.Applicative
  , module Control.Monad
  , module Control.Monad.Identity
  , module Control.Monad.Reader
  , module Control.Monad.State.Strict
  , module Control.Monad.Trans
  , module Data.ByteString
  , module Data.Foldable
  , module Data.HashMap.Strict
  , module Data.Maybe
  , module Data.Monoid
  , module Data.Set
  , module Data.Text
  , module Network.Http.Client
  , module Prelude
  , module Text.Str
  , timeFmatShort, timeFmatLong, timeFmatHttpDate, hexHash, hmac256
  , sortByKey, encodeUri, (~>), addSlash, joinUri, charToHex
  ) where

import qualified Prelude as P
import Prelude ( IO, Char, Monad(..), Functor(..), Bool(..), Ord(..), Eq(..)
               , Show
               , Maybe(..), Either(..)
               , or, otherwise, fst, id, error, not, map, print, filter
               , uncurry, flip
               , (<), (>), (<=), (>=), (=<<), (&&), (||), ($), (.))
import Codec.Utils (Octet)
import Control.Applicative (Applicative(..), (<$>), (<*))
import Control.Monad (when)
import Control.Monad.Trans (MonadIO(..), lift)
import Control.Monad.Identity (Identity(..))
import Control.Monad.Reader(ReaderT(..), MonadReader(..))
import Control.Monad.State.Strict (StateT(..), execStateT, modify, gets)
import Crypto.Hash (SHA256(..), hmacAlg)
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Byteable (toBytes)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import Data.Foldable (foldl1, forM_)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.List as L
import Data.Monoid
import Data.Maybe (maybe, isJust, isNothing, fromJust)
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import Network.Http.Client ( Method(..), Request, Response, Hostname, Port
                           , buildRequest, http, setHostname, setHeader
                           )
import System.Environment (getEnvironment)
import System.Time (getClockTime, toUTCTime, ctTZName, formatCalendarTime)
import System.Locale (defaultTimeLocale)
import Text.Str hiding (error)
import Text.URI (URI(..), mergeURIs)

---------------------------------------------------------------------
-- Utility functions
---------------------------------------------------------------------

-- | Given a format string, gets the current time and formats the string.
currentTimeFmat :: (MonadIO io, Str s) => s -> io s
currentTimeFmat (toString -> str) = do 
  time <- liftIO getClockTime
  let timeUTC = (toUTCTime time){ctTZName="GMT"}
  return $ fromString $ formatCalendarTime defaultTimeLocale str timeUTC

-- | The "short format" string AWS expects
timeFmatShort :: (Str s, MonadIO io) => io s
timeFmatShort = currentTimeFmat "%Y%m%d" -- "20130524"

-- | The "long format" string AWS expects
timeFmatLong :: (Str s, MonadIO io) => io s
timeFmatLong = currentTimeFmat "%Y%m%dT%H%M%SZ" -- "20130524T000000Z"

-- | The time format for a Date header.
timeFmatHttpDate :: (Str s, MonadIO io) => io s
timeFmatHttpDate = currentTimeFmat "%a, %d %B %Y %H:%M:%S %Z"

-- | Joins two URI strings together.
joinUri :: Str s => s -> s -> s
joinUri = (<>)

-- | Hashes with HMAC, using the given secret key.
hmac256 :: ByteString -> ByteString -> ByteString
hmac256 key msg = toBytes $ hmacAlg SHA256 key msg

-- | Hashes with SHA256, and then encodes in Base16.
hexHash :: Str s => s -> s
hexHash = asByteString $ toHex . SHA256.hash

-- | Left-to-right function composition
(~>) :: (a -> b) -> (b -> c) -> a -> c
(~>) = P.flip (.)
infixl 9 ~>

-- | Sorts a list of tuples by the first tuple
sortByKey :: Ord a => [(a, b)] -> [(a, b)]
sortByKey = L.sortBy $ \a b -> fst a `compare` fst b

-- | Adds a preceding forward slash if not already present.
addSlash :: Str s => s -> s
addSlash = asString $ \case {'/':s -> '/':s; s -> '/':s}

-- | Returns a properly-encoded URI string. @encodeSlash@ determines whether
-- a slash will be left as-is (if False), or replaced with @%2F@.
encodeUri :: Str s => Bool -> s -> s
encodeUri encodeSlash = asString go where
  go "" = ""
  go (c:cs) | okAsIs c = c : go cs
            | otherwise = charToHex c <> go cs
  okAsIs c = or [ c >= 'A' && c <= 'Z', c >= 'a' && c <= 'z', c == '_'
                , c >= '0' && c <= '9', c == '-', c == '~', c == '.'
                , not encodeSlash && c == '/']

-- | Converts a character to @"%H"@, where @H@ is the representation in hex.
-- For example, @charToHex ' ' == "%20"@, and @charToHex '/' == "%2F"@.
charToHex :: Str s => Char -> s
charToHex = fromByteString . upper . cons '%' . toHex . singleton
