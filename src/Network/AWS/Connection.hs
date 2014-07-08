{-# LANGUAGE OverloadedStrings,
             RecordWildCards,
             LambdaCase,
             ViewPatterns #-}
module Network.AWS.Connection where

import qualified Prelude as P
import Prelude ( IO, Monad(..), Functor(..), Bool(..), Ord(..), Eq(..)
               , Show
               , Maybe(..), Either(..)
               , or, otherwise, fst, id, error
               , (<), (>), (<=), (>=), (&&), (||), ($), (.))
import Control.Applicative (Applicative(..), (<$>), (<*))
import Data.Monoid
import Data.Maybe (maybe)
import Data.String (IsString(..))
import Network.Http.Client (Method(..), Request, Response, buildRequest
                           , http)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment
import System.Time
import System.Locale
import Text.Str hiding (error)
import Text.URI (URI(..), mergeURIs)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Base16 as B16

-- | Stores the key id and secret key for an AWS transaction.
data AwsCredentials str = AwsCredentials
  { awsKeyId :: str
  , awsSecretKey :: str
  } deriving (Show)

-- | Configures commonly-used AWS options.
data AwsConfig str = AwsConfig
  { awsHostName :: str
  , awsRegion :: str
  , awsIsSecure :: Bool
  , awsGivenCredentials :: CredentialSource str
  } deriving (Show)

-- | Credentials can either be supplied directly, provided through a file,
-- or given in the environment.
data CredentialSource s = FromEnv
                        | FromFile s
                        | Directly (AwsCredentials s) 
                        deriving (Show)

-- | Combines static config from the user with credentials which might be
-- determined at run-time.
data AwsConnection str = AwsConnection
  { awsConfig :: AwsConfig str
  , awsCredentials :: AwsCredentials str
  } deriving (Show)

-- | All of the information needed for an S3 command. The type variable must
-- be of the @Str@ class.
data S3Command str = S3Command
  { s3Method :: Method
  , s3Connection :: AwsConnection str
  , s3Bucket :: str
  , s3Object :: str
  , s3Query :: [(str, Maybe str)]
  , s3Headers :: [(str, str)]
  , s3Body :: str
  } deriving (Show)

-- | Some common options
defaultConfig :: Str s => AwsConfig s
defaultConfig = AwsConfig { awsHostName = "s3.amazonaws.com"
                          , awsRegion = "us-east-1"
                          , awsIsSecure = True
                          , awsGivenCredentials = FromEnv }

accessIdKey :: P.String
accessIdKey = "AWS_ACCESS_KEY_ID"

secretKeyKey :: P.String
secretKeyKey = "AWS_SECRET_ACCESS_KEY"


-- | Loads credentials from the environment
autoOpts :: Str s => AwsConfig s -> IO (AwsConnection s)
autoOpts cfg@(AwsConfig{..}) = AwsConnection cfg <$> creds where
  creds = case awsGivenCredentials of
    Directly creds -> return creds
    FromFile file -> findCredentialsFromFile file >>= \case
      Just creds -> return creds
      Nothing -> error $ unlines [ "Couldn't get credentials from file " 
                                   <> show file <> "."
                                 , "The file should use ConfigFile format, "
                                   <> "and have the keys " <> show accessIdKey
                                   <> " (access id) and " <> show secretKeyKey
                                   <> " (secred access key)."]
    FromEnv -> findCredentialsFromEnv >>= \case
      Just creds -> return creds
      Nothing -> error $ unlines [ "No credentials found in environment."
                                 , "An access key id should be under the "
                                   <> "variable " <> show accessIdKey <> "."
                                 , "A secret access key should be under the "
                                   <> "variable " <> show secretKeyKey <> "."]

-- | Produces a base s3 command. Should not be used externally.
s3Command :: Str s => AwsConnection s -> S3Command s
s3Command con = S3Command GET con "" "" [] [] ""

s3GetCmd :: Str s => AwsConnection s -> s -> s -> S3Command s
s3GetCmd con bucket object = cmd {s3Bucket=bucket,s3Object=object}
  where cmd = s3Command con

findCredentialsFromEnv :: Str s => IO (Maybe (AwsCredentials s))
findCredentialsFromEnv = getEnvKey accessIdKey >>= \case
  Nothing -> return Nothing
  Just i -> getEnvKey secretKeyKey >>= \case
    Just k -> return $ Just $ AwsCredentials i k
    Nothing -> getEnvKey "AWS_ACCESS_KEY_SECRET" >>= \case
      Just k -> return $ Just $ AwsCredentials i k
      Nothing -> return Nothing
  where getEnvKey s = fmap fromString . L.lookup s <$> getEnvironment

findCredentialsFromFile :: Str s => s -> IO (Maybe (AwsCredentials s))
findCredentialsFromFile (toString -> path) = P.undefined

s3Request :: Str str => S3Command str -> IO Request
s3Request S3Command{..} = P.undefined -- buildRequest $ do
  --http s3Method $ "/" <> s3Object
  --P.undefined

canonicalRequest :: Str s => S3Command s -> s
canonicalRequest S3Command{..} = joinLines parts where
  parts = [method, uri, query, headers, headerKeys, payload]
  sortByKey = L.sortBy $ \a b -> fst a `compare` fst b
  encodeKV (k, v) = encodeUri True k <> "=" <> maybe "" (encodeUri True) v
  addSlash = asString $ \case {'/':s -> '/':s; s -> '/':s}
  method = show s3Method
  uri = encodeUri False $ joinUri (addSlash s3Bucket) s3Object
  query = joinBy "&" $ fmap encodeKV $ sortByKey s3Query
  headers = joinLines $ fmap (\(k, v) -> k <> ":" <> v) $ sortByKey s3Headers
  headerKeys = joinSemis $ L.sortBy compare $ fmap fst s3Headers
  payload = asByteString (B16.encode . SHA256.hash) s3Body

-- | Implementation of AWS's rules for constructing the string to sign
stringToSign :: Str s => S3Command s -> IO s
stringToSign cmd@(S3Command{..}) = joinLines <$> parts where
  parts = do stamp <- mkStamp
             scope <- mkScope
             return ["AWS4-HMAC-SHA256", stamp, scope, canonicalRequest cmd]
  utcTime c = (toUTCTime c) {ctTZName = "GMT"}
  format str = formatCalendarTime defaultTimeLocale str . utcTime
  mkStamp = fromString . format "%Y-%m-%d" <$> getClockTime
  mkScope = do
    date <- fromString . format "%Y%m%d" <$> getClockTime
    let region = awsRegion $ awsConfig s3Connection
    return $ joinSlashes [date, region, "s3", "aws4_request"]
  
-- | Joins two URI strings together.
joinUri :: Str s => s -> s -> s
joinUri = (<>)

-- | Returns a properly-encoded URI string. @encodeSlash@ determines whether
-- a slash will be left as-is, or replaced with @%2F@.
encodeUri :: Str s => Bool -> s -> s
encodeUri encodeSlash = asString go where
  go "" = ""
  go (c:cs) | ok c = c : go cs
            | otherwise = convert c <> go cs
  ok c = or [ c >= 'A' && c <= 'Z', c >= 'a' && c <= 'z', c == '_'
            , c >= '0' && c <= '9', c == '-', c == '~', c == '.'
            , encodeSlash && c == '/']
  convert '/' = "%2F"
  convert ' ' = "%20"
  convert c = error $ "Can't encode character " <> show c <> " in URI"
