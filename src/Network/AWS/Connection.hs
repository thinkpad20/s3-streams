{-# LANGUAGE OverloadedStrings,
             RecordWildCards,
             LambdaCase,
             ViewPatterns,
             ScopedTypeVariables #-}
module Network.AWS.Connection where

import qualified Prelude as P
import Prelude ( IO, Char, Monad(..), Functor(..), Bool(..), Ord(..), Eq(..)
               , Show
               , Maybe(..), Either(..)
               , or, otherwise, fst, id, error, not, map
               , (<), (>), (<=), (>=), (&&), (||), ($), (.))
import Codec.Utils (Octet)
import Control.Applicative (Applicative(..), (<$>), (<*))
import Crypto.Hash (SHA256(..), hmacAlg)
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Byteable (toBytes)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import Data.Char (toUpper, toLower)
import Data.Foldable (foldl1)
import qualified Data.List as L
import Data.Monoid
import Data.Maybe (maybe)
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import Network.Http.Client (Method(..), Request, Response, buildRequest, http)
import System.Environment (getEnvironment)
import System.Time (getClockTime, toUTCTime, ctTZName, formatCalendarTime)
import System.Locale (defaultTimeLocale)
import Text.Str hiding (error)
import Text.URI (URI(..), mergeURIs)

---------------------------------------------------------------------
-- Data types
---------------------------------------------------------------------

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
  , awsService :: str
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

---------------------------------------------------------------------
-- Loading configurations
---------------------------------------------------------------------

-- | The key (either in the env or in a config file) for the AWS access key.
accessIdKey :: P.String
accessIdKey = "AWS_ACCESS_KEY_ID"

-- | The key (either in the env or in a config file) for the AWS secret key.
secretKeyKey :: P.String
secretKeyKey = "AWS_SECRET_ACCESS_KEY"

-- | Creates an AwsConnection using a config.
createConnection :: Str s => AwsConfig s -> IO (AwsConnection s)
createConnection cfg@(AwsConfig{..}) = AwsConnection cfg <$> creds where
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

-- | Chooses some reasonably sane defaults.
defaultConfig :: Str s => AwsConfig s
defaultConfig = AwsConfig { awsHostName = "s3.amazonaws.com"
                          , awsRegion = "us-east-1"
                          , awsService = "s3"
                          , awsIsSecure = True
                          , awsGivenCredentials = FromEnv }

-- | Uses the default config to create an AwsConnection.
defaultConnection :: Str s => IO (AwsConnection s)
defaultConnection = createConnection defaultConfig

-- | Looks for credentials in the environment.
findCredentialsFromEnv :: Str s => IO (Maybe (AwsCredentials s))
findCredentialsFromEnv = getEnvKey accessIdKey >>= \case
  Nothing -> return Nothing
  Just i -> getEnvKey secretKeyKey >>= \case
    Just k -> return $ Just $ AwsCredentials i k
    Nothing -> getEnvKey "AWS_ACCESS_KEY_SECRET" >>= \case
      Just k -> return $ Just $ AwsCredentials i k
      Nothing -> return Nothing
  where getEnvKey s = fmap fromString . L.lookup s <$> getEnvironment

-- | Looks for credentials in a config file.
findCredentialsFromFile :: Str s => s -> IO (Maybe (AwsCredentials s))
findCredentialsFromFile (toString -> path) = P.undefined

---------------------------------------------------------------------
-- S3 Command wrappers
---------------------------------------------------------------------

-- | Produces a base s3 command. Should not be used externally.
s3Command :: Str s => AwsConnection s -> S3Command s
s3Command con = S3Command GET con "" "" [] [] ""

-- | An S3 GET command, given a bucket and an object.
s3GetCmd :: Str s => AwsConnection s -> s -> s -> S3Command s
s3GetCmd con bucket object = cmd {s3Bucket=bucket,s3Object=object}
  where cmd = s3Command con

s3Request :: Str str => S3Command str -> IO Request
s3Request S3Command{..} = P.undefined -- buildRequest $ do
  --http s3Method $ "/" <> s3Object
  --P.undefined

---------------------------------------------------------------------
-- Building request signature
-- There are precise rules for how to build, and in particular sign, an 
-- S3 request. For the full rules, from which these implementations are
-- adapted, see: http://docs.aws.amazon.com/AmazonS3/latest/API/
-- sig-v4-header-based-auth.html.
---------------------------------------------------------------------

-- | Adds some headers that we need to our S3 request.
addHeaders :: Str s => S3Command s -> IO (S3Command s)
addHeaders cmd = do
  let host = awsHostName $ awsConfig $ s3Connection cmd
      bucket = s3Bucket cmd
      bodySha = hexHash $ s3Body cmd
  date <- timeFmatLong
  let headers = [ ("host", bucket <> "." <> host)
                , ("range", "bytes=0-9")
                , ("x-amz-content-sha256", bodySha)
                , ("x-amz-date", date)]
  return cmd {s3Headers = headers <> s3Headers cmd}

-- | Munges information from the command into a "canonical request".
canonicalRequest :: Str s => S3Command s -> IO s
canonicalRequest cmd = do
  S3Command{..} <- addHeaders cmd
  let sortByKey = L.sortBy $ \a b -> fst a `compare` fst b
      doKey = encodeUri True . smap toLower
      doVal = maybe "" (encodeUri True . trim)
      encodeKV (k, v) = doKey k <> "=" <> doVal v
      addSlash = asString $ \case {'/':s -> '/':s; s -> '/':s}
      uri = encodeUri False $ addSlash s3Object
      query = joinBy "&" $ fmap encodeKV $ sortByKey s3Query
      putColon (k, v) = k <> ":" <> v
      headers = unlines $ fmap putColon $ sortByKey s3Headers
      headerKeys = joinSemis $ L.sortBy compare $ fmap fst s3Headers
      bodyHash = hexHash s3Body
      method = show s3Method
  return $ joinLines [method, uri, query, headers, headerKeys, bodyHash]

-- | Implementation of AWS's rules for constructing the string to sign
stringToSign :: Str s => S3Command s -> IO s
stringToSign cmd@(S3Command{..}) = joinLines <$> parts where
  parts = do 
    stamp <- timeFmatLong
    scope <- do
      date <- timeFmatShort
      let region = awsRegion $ awsConfig s3Connection
      return $ joinSlashes [date, region, "s3", "aws4_request"]
    request <- canonicalRequest cmd
    return ["AWS4-HMAC-SHA256", stamp, scope, hexHash request]

-- | Computes a signature according to the Version 4 rules.
v4Signature :: Str s => AwsConnection s -> IO s
v4Signature AwsConnection{..} = fromByteString <$> do
  date <- timeFmatShort
  let keys = map toByteString ["AWS4" <> awsSecretKey awsCredentials
                              , date, awsRegion awsConfig
                              , awsService awsConfig, "aws4_request"]
  return $ B16.encode $ foldl1 hmac256 keys

-- | Hashes with HMAC, using the given secret key.
hmac256 :: ByteString -> ByteString -> ByteString
hmac256 key msg = toBytes $ hmacAlg SHA256 key msg

-- | Hashes with SHA256, and then encodes in Base16
hexHash :: Str s => s -> s
hexHash = asByteString $ B16.encode . SHA256.hash

---------------------------------------------------------------------
-- Utility functions
---------------------------------------------------------------------

-- | Given a format string, gets the current time and formats the string.
currentTimeFmat :: Str s => s -> IO s
currentTimeFmat (toString -> str) = do 
  time <- getClockTime
  let timeUTC = (toUTCTime time){ctTZName="GMT"}
  return $ fromString $ formatCalendarTime defaultTimeLocale str timeUTC

-- | The "short format" string AWS expects
timeFmatShort :: Str s => IO s
timeFmatShort = currentTimeFmat "20130524" -- "%Y%m%d"

-- | The "long format" string AWS expects
timeFmatLong :: Str s => IO s
timeFmatLong = currentTimeFmat "20130524T000000Z" -- "%Y%m%dT000000Z"

-- | Joins two URI strings together.
joinUri :: Str s => s -> s -> s
joinUri = (<>)

-- | Returns a properly-encoded URI string. @encodeSlash@ determines whether
-- a slash will be left as-is (if False), or replaced with @%2F@.
encodeUri :: Str s => Bool -> s -> s
encodeUri encodeSlash = asString go where
  go "" = ""
  go (c:cs) | ok c = c : go cs
            | otherwise = charToHex c <> go cs
  ok c = or [ c >= 'A' && c <= 'Z', c >= 'a' && c <= 'z', c == '_'
            , c >= '0' && c <= '9', c == '-', c == '~', c == '.'
            , not encodeSlash && c == '/']

-- | Converts a character to @"%H"@, where @H@ is the representation in hex.
-- For example, @charToHex ' ' == "%20"@, and @charToHex '/' == "%2F"@.
charToHex :: Str s => Char -> s
charToHex = fromByteString . smap toUpper . cons '%' . B16.encode . singleton

---------------------------------------------------------------------
-- Tests (for debugging)
---------------------------------------------------------------------

testSecret :: Text
testSecret = "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"

testKeyId :: Text
testKeyId = "AKIAIOSFODNN7EXAMPLE"

testConnection :: IO (AwsConnection Text)
testConnection = createConnection config where
  creds = AwsCredentials testKeyId testSecret
  config = defaultConfig {awsGivenCredentials = Directly creds}

testCommand :: IO (S3Command Text)
testCommand = do con <- testConnection
                 return $ s3GetCmd con bucket object
  where bucket = "examplebucket"
        object = "test.txt"
