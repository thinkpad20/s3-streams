{-# LANGUAGE OverloadedStrings,
             RecordWildCards,
             LambdaCase #-}
module Network.AWS.Connection where

import qualified Prelude as P
import Prelude ( IO, Monad(..), Functor(..), Bool(..), Ord(..), Eq(..)
               , Show, (++)
               , or, otherwise
               , (<), (>), (<=), (>=), (&&), (||), (:), ($))
import Control.Applicative (Applicative(..), (<$>), (<*))
import Data.Monoid
import Data.String (IsString(..))
import Network.Http.Types (StdMethod(..))
import Network.Http.Connection (Request, Response)
import Network.Http.RequestBuilder (buildRequest)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import Text.URI (URI(..), mergeURIs)

-- | Extends the IsString class with the ability to go the other way.
class IsString s => FullString s where 
  toString :: s -> String
  intercalate :: s -> [s] -> s
  intercalate s ss = fromString $ L.intercalate s (toString <$> ss)

instance FullString String where 
  toString = id
  
instance FullString ByteString where 
  toString = B.unpack

instance FullString Text where 
  toString = T.unpack
  intercalate = T.intercalate

show :: (Show a, FullString s) => a -> s
show = fromString . P.show

asString :: FullString s => (String -> String) -> s -> s
asString func = fromString . func . toString

asString2 :: FullString s => (String -> String -> String) -> s -> s -> s
asString2 f s1 s2 = fromString $ f (toString s1) (toString s2) 

data AwsCredentials = AwsCredentials
  { awsKeyId :: ByteString
  , awsSecretKey :: ByteString }
  deriving (Show)

data AwsConnectOpts txt = AwsConnectOpts
  { acoIsSecure :: Bool
  , acoCredentials :: AwsCredentials
  , acoHostName :: Maybe txt
  }

findCredentials :: IO (Either Text AwsCredentials)
findCredentials = undefined

-- | All of the information needed for an S3 command. The type variable is some 
-- textual type, like Text, String or ByteString.
data S3Command txt = S3Command
  { s3Method :: StdMethod
  , s3Options :: AwsConnectOpts
  , s3Bucket :: txt
  , s3Object :: txt
  , s3Query :: txt
  , s3Headers :: [(txt, txt)]
  , s3Body :: Maybe txt
  , s3IsSecure :: Bool
  } deriving (Show)

s3Request :: S3Command -> IO Request
s3Request S3Command{..} = buildRequest $ do
  http s3method $ "/" <> s3object
  undefined

canonicalRequest :: FullString s => S3Command s -> s
canonicalRequest S3Command{..} = fromString $ intercalate "\n" parts where
  parts = [method, uri, query, cHeaders, sHeaders, hPayload]
  method = show s3method
  uri = encodeUri False $ joinUri (addSlash s3Bucket) s3Object
  query = s3Query


addSlash :: FullString s => s -> s
addSlash = asString $ \case
  '/':s -> '/':s
  s -> '/':s

-- | Joins two URI strings together.
joinUri :: FullString s => s -> s -> s
joinUri = asString2 (++)

-- | Returns a properly-encoded URI string. @encodeSlash@ determines whether
-- a slash will be left as-is, or replaced with @%2F@.
encodeUri :: FullString s => Bool -> s -> s
encodeUri encodeSlash = asString go where
  go "" = ""
  go (c:cs) | ok c = c : go cs
            | otherwise = convert c <> go cs
  ok c = or [ c >= 'A' && c <= 'Z', c >= 'a' && ch <= 'z', c == '_'
            , c >= '0' && c <= '9', c == '-', ch == '~', ch == '.'
            , encodeSlash && c == '/']
  convert '/' = "%2F"
  convert ' ' = "%20"
  convert c = error $ "Can't encode character " <> show c <> " in URI"
