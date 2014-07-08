{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
module Text.Str (
    Str(..)
  , show
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
import Prelude (Show, String, Char, Ord(..), (.), id, ($))
import Data.Monoid
import Data.String (IsString(..))
import qualified Data.List as L
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.ByteString.Char8 as B
import Data.ByteString (ByteString)

-- | Extends the IsString class with the ability to go the other way.
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

show :: (Show a, Str s) => a -> s
show = fromString . P.show

asString :: Str s => (String -> String) -> s -> s
asString func = fromString . func . toString

asByteString :: Str s => (ByteString -> ByteString) -> s -> s
asByteString func = fromByteString . func . toByteString

asText :: Str s => (Text -> Text) -> s -> s
asText func = fromText . func . toText

asString2 :: Str s => (String -> String -> String) -> s -> s -> s
asString2 f s1 s2 = fromString $ f (toString s1) (toString s2)

unlines :: Str s => [s] -> s
unlines s = joinLines s `snoc` '\n' 

joinLines :: Str s => [s] -> s
joinLines = joinBy "\n"

joinCommas :: Str s => [s] -> s
joinCommas = joinBy ","

joinSemis :: Str s => [s] -> s
joinSemis = joinBy ";"

joinSlashes :: Str s => [s] -> s
joinSlashes = joinBy "/"
