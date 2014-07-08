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
  , asString2
  , asByteString
  , asText
  , unlines
  ) where

import qualified Prelude as P
import Prelude (IO, Show, String, Char, Ord(..), (.), id, ($))
import Data.Monoid
import Data.String (IsString(..))
import qualified Data.List as L
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.ByteString.Char8 as B
import Data.ByteString (ByteString)

-- | Extends the @IsString@ class with the ability to go the other way,
-- allowing the abstraction of a particular String type, such that functions
-- can be written to more easily accept any of the common string types,
-- such as String, ByteString (and its variants), Text, etc. It also makes
-- @newtype@-wrapped strings easier to work with.
class (IsString s, Show s, Ord s, Monoid s) => Str s where 
  toString :: s -> String
  toByteString :: s -> ByteString
  toText :: s -> Text
  fromText :: Text -> s
  fromByteString :: ByteString -> s
  joinBy :: s -> [s] -> s
  cons :: Char -> s -> s
  snoc :: s -> Char -> s

instance Str String where 
  toString = id
  toByteString = B.pack
  toText = T.pack
  fromByteString = B.unpack
  fromText = T.unpack
  joinBy = L.intercalate
  cons = (:)
  snoc s c = s <> [c]

instance Str ByteString where 
  toString = B.unpack
  toByteString = id
  toText = T.pack . B.unpack
  fromByteString = id
  fromText = B.pack . T.unpack
  joinBy = B.intercalate
  cons = B.cons
  snoc = B.snoc

instance Str Text where 
  toString = T.unpack
  toByteString = B.pack . T.unpack
  toText = id
  fromByteString = T.pack . B.unpack
  fromText = id
  joinBy = T.intercalate
  cons = T.cons
  snoc = T.snoc

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

-- | Same as @asString@ but for functions with arity 2.
asString2 :: Str s => (String -> String -> String) -> s -> s -> s
asString2 f s1 s2 = fromString $ f (toString s1) (toString s2)

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
