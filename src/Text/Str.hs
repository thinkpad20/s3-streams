{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
module Text.Str (
    Str(..)
  , show
  , error
  , joinLines
  , joinCommas
  , joinSemis
  , joinSlashes
  , asString
  , asByteString
  , asText
  , asString2
  , unlines
  , putStrLn
  ) where

import qualified Prelude as P
import Prelude (IO, Show, String, Char, Bool, Ord(..), (.), id, ($), flip)
import qualified Codec.Binary.UTF8.String as US
import Data.Char (isSpace)
import Data.Monoid
import Data.String (IsString(..))
import qualified Data.List as L
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Codec.Utils (Octet)

instance IsString [Octet] where
  fromString = US.encode

-- | Extends the @IsString@ class with the ability to go the other way,
-- allowing the abstraction of a particular String type, such that functions
-- can be written to more easily accept any of the common string types,
-- such as String, ByteString (and its variants), Text, etc. It also makes
-- @newtype@-wrapped strings easier to work with.
class (IsString s, Show s, Ord s, Monoid s) => Str s where 
  toString :: s -> String
  toByteString :: s -> ByteString
  toText :: s -> Text
  toOctets :: s -> [Octet]
  fromText :: Text -> s
  fromByteString :: ByteString -> s
  fromOctets :: [Octet] -> s
  fromOctets = fromString . US.decode
  joinBy :: s -> [s] -> s
  cmap :: (Char -> Char) -> s -> s
  singleton :: Char -> s
  cons :: Char -> s -> s
  snoc :: s -> Char -> s
  reverse :: s -> s
  dropWhile :: (Char -> Bool) -> s -> s
  trim :: s -> s
  trim = f . f where f = reverse . dropWhile isSpace

instance Str String where 
  toString = id
  toByteString = BC.pack
  toText = T.pack
  toOctets = US.encode
  fromByteString = BC.unpack
  fromText = T.unpack
  joinBy = L.intercalate
  cmap = P.map
  singleton c = [c]
  cons = (:)
  snoc s c = s <> [c]
  reverse = P.reverse
  dropWhile = P.dropWhile

instance Str ByteString where 
  toString = BC.unpack
  toByteString = id
  toText = decodeUtf8
  toOctets = B.unpack
  fromByteString = id
  fromText = encodeUtf8
  cmap = BC.map
  joinBy = BC.intercalate
  singleton = BC.singleton
  cons = BC.cons
  snoc = BC.snoc
  reverse = BC.reverse
  dropWhile = BC.dropWhile

instance Str Text where 
  toString = T.unpack
  toByteString = encodeUtf8
  toText = id
  toOctets = US.encode . toString
  fromByteString = decodeUtf8
  fromText = id
  fromOctets = fromString . US.decode
  cmap = T.map
  joinBy = T.intercalate
  singleton = T.singleton
  cons = T.cons
  snoc = T.snoc
  reverse = T.reverse
  dropWhile = T.dropWhile

--instance Str [Octet] where
--  toString = US.decode
--  toByteString = B.pack . toString
--  toText = T.pack . toString
--  fromByteString = fromString . B.unpack
--  fromText = fromString . T.unpack
--  cmap f = fromString . cmap f . toString
--  joinBy = L.intercalate
--  singleton c = fromString [c]
--  cons c = fromString . cons c . toString
--  snoc os c = fromText . flip snoc c . toText $ os
--  reverse = P.reverse
--  dropWhile test = fromString . P.dropWhile test . toString

-- | Generalizes @show@ to return any string type.
show :: (Show a, Str s) => a -> s
show = fromString . P.show

-- | Generalizes @error@ to accept any string type.
error :: Str s => s -> a
error = wrapString P.error

-- | Generalizes @putStrLn@.
putStrLn :: Str s => s -> IO ()
putStrLn = wrapString P.putStrLn

-- | Generalizes functions that take a @String@.
wrapString :: Str s => (String -> a) -> s -> a
wrapString f = f . toString

-- | Generalizes functions that take a @ByteString@.
wrapByteString :: Str s => (ByteString -> a) -> s -> a
wrapByteString f = f . toByteString

-- | Converts a function that takes a @Text@ into one that takes any @Str@.
wrapText :: Str s => (Text -> a) -> s -> a
wrapText f = f . toText

-- | Converts a function that operates on @String@s to one that operates
-- on any @Str@.
asString :: Str s => (String -> String) -> s -> s
asString func = fromString . func . toString

-- | Converts a function that operates on @ByteString@s to one that operates
-- on any @Str@.
asByteString :: Str s => (ByteString -> ByteString) -> s -> s
asByteString func = fromByteString . func . toByteString

-- | Converts a function that operates on @Text@ to one that operates on any
-- @Str@.
asText :: Str s => (Text -> Text) -> s -> s
asText func = fromText . func . toText

--asOctets :: Str s => ([Octet] -> [Octet]) -> s -> s
--asOctets f = fromOctets . f . toOctets

-- | Same as @asString@ but for functions with arity 2.
asString2 :: Str s => (String -> String -> String) -> s -> s -> s
asString2 f s1 s2 = fromString $ f (toString s1) (toString s2)

--asOctets2 :: Str s => ([Octet] -> [Octet] -> [Octet]) -> s -> s -> s
--asOctets2 f s1 s2 = fromOctets $ f (toOctets s1) (toOctets s2)

--toOctets :: Str s => s -> [Octet]
--toOctets = US.encode . toString

-- | Joins strings with newline separation, and adds a trailing newline.
unlines :: Str s => [s] -> s
unlines s = joinLines s `snoc` '\n'

-- | Joins strings with newlines.
joinLines :: Str s => [s] -> s
joinLines = joinBy "\n"

-- | Joins strings with commas.
joinCommas :: Str s => [s] -> s
joinCommas = joinBy ","

-- | Joins strings with semicolons.
joinSemis :: Str s => [s] -> s
joinSemis = joinBy ";"

-- | Joins strings with forward slashes.
joinSlashes :: Str s => [s] -> s
joinSlashes = joinBy "/"
