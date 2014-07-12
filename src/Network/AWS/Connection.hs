{-# LANGUAGE OverloadedStrings,
             RecordWildCards,
             LambdaCase,
             ViewPatterns,
             ScopedTypeVariables,
             NoMonomorphismRestriction #-}
module Network.AWS.Connection where

import qualified Prelude as P
import Prelude ( IO, Char, Monad(..), Functor(..), Bool(..), Ord(..), Eq(..)
               , Show
               , Maybe(..), Either(..)
               , or, otherwise, fst, id, error, not, map, print, filter
               , uncurry
               , (<), (>), (<=), (>=), (=<<), (&&), (||), ($), (.))
import Codec.Utils (Octet)
import Control.Applicative (Applicative(..), (<$>), (<*))
import Control.Monad (when)
import Control.Monad.Trans (MonadIO(..), lift)
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
-- AWS Type Classes
-- Abstracts some common capabilities of AWS-related types.
---------------------------------------------------------------------

-- | Class of things which contain AWS configuration.
class Aws a where
  getCon :: Str s => a s -> AwsConnection s

-- | The class of types from which we can generate CanonicalHeaders.
class Aws a => Canonical a where 
  -- | Builds the canonical list of headers.
  canonicalHeaders :: Str s => a s -> [(s, s)]
  -- | Munges information from the command into a "canonical request".
  canonicalRequest :: Str s => a s -> s

-- | Class of things that can be made into requests.
class Req a where
  getMethod :: Str s => a s -> Method
  getHeaders :: Str s => a s -> [(s, s)]
  getHost :: Str s => a s -> s
  getPort :: Str s => a s -> Port
  getUri :: Str s => a s -> s

---------------------------------------------------------------------
-- Data types
-- The @str@ in these types must be a type that implements @Str@.
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
  , s3Headers :: HashMap str str
  , s3ExcludeHeaders :: Set str
  , s3Body :: str
  } deriving (Show)

instance Aws AwsConnection where
  getCon = id

instance Aws S3Command where
  getCon = s3Connection
  
instance Req S3Command where
  getMethod = s3Method
  getHeaders S3Command{..} = M.toList s3Headers
  getHost cmd = s3Bucket cmd <> "." <> getHostName cmd
  getPort _ = 80
  getUri S3Command{..} = addSlash s3Object
  
-- | Various getters for Aws types:
getCreds :: (Aws aws, Str s) => aws s -> AwsCredentials s
getCreds = awsCredentials . getCon
getConfig :: (Aws aws, Str s) => aws s -> AwsConfig s
getConfig = awsConfig . getCon
getHostName :: (Aws aws, Str s) => aws s -> s
getHostName = awsHostName . getConfig
getRegion :: (Aws aws, Str s) => aws s -> s
getRegion = awsRegion . getConfig
getSecurity :: (Aws aws, Str s) => aws s -> Bool
getSecurity = awsIsSecure . getConfig
getService :: (Aws aws, Str s) => aws s -> s
getService = awsService . getConfig

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
createConnection :: (Functor io, MonadIO io, Str s) 
                 => AwsConfig s -> io (AwsConnection s)
createConnection cfg@AwsConfig{..} = AwsConnection cfg <$> creds where
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
defaultConnection :: (Functor io, MonadIO io, Str s) => io (AwsConnection s)
defaultConnection = createConnection defaultConfig

-- | Looks for credentials in the environment.
findCredentialsFromEnv :: (Functor io, MonadIO io, Str s) 
                       => io (Maybe (AwsCredentials s))
findCredentialsFromEnv = do
  let getEnvKey s = liftIO $ fmap fromString . L.lookup s <$> getEnvironment
  keyid <- getEnvKey accessIdKey
  secret <- getEnvKey secretKeyKey
  if isNothing keyid || isNothing secret then return Nothing
  else return $ Just $ AwsCredentials (fromJust keyid) (fromJust secret)

-- | Looks for credentials in a config file.
findCredentialsFromFile :: (Functor io, MonadIO io, Str s) 
                        => s -> io (Maybe (AwsCredentials s))
findCredentialsFromFile (toString -> path) = P.undefined

---------------------------------------------------------------------
-- Building S3 Commands
---------------------------------------------------------------------

type S3Builder s m = StateT (S3Command s) m
type S3Builder' s m = S3Builder s m ()

-- | Builds an S3 command with a series of builder actions, starting with a 
-- connection.
buildCommand :: (Str s, Monad m) 
             => AwsConnection s -> S3Builder s m a -> m (S3Command s)
buildCommand con steps = execStateT steps $ s3Command con

-- | Produces a base s3 command. Should not be used externally.
s3Command :: Str s => AwsConnection s -> S3Command s
s3Command con = S3Command GET con e e e e e e where e = mempty

-- | An S3 GET command, given a bucket and an object.
s3GetCmd :: Str s => AwsConnection s -> s -> s -> S3Command s
s3GetCmd con bucket object = cmd {s3Bucket=bucket,s3Object=object}
  where cmd = s3Command con

-- | Sets the S3 method.
setMethod :: (Str s, Monad m) => Method -> S3Builder' s m
setMethod m = modify $ \c -> c {s3Method = m}

-- | Sets the S3 bucket.
setBucket :: (Str s, Monad m) => s -> S3Builder' s m
setBucket bucket = modify $ \c -> c {s3Bucket = bucket}

-- | Sets the S3 method.
setObject :: (Str s, Monad m) => s -> S3Builder' s m
setObject object = do
  modify $ \c -> c {s3Object = addSlash object}

-- | Adds an arbitrary header.
addHeader :: (Str s, Monad m) => s -> s -> S3Builder s m ()
addHeader h val = modify $ \c -> c {s3Headers = M.insert h val $ s3Headers c}

-- | Adds the date header, set with the current date.
addAmzDateHeader :: (MonadIO io, Str s) => S3Builder s io ()
addAmzDateHeader = lift timeFmatLong >>= addHeader "x-amz-date"

-- | Computes the hash of the S3 body and adds the content-sha header.
addContentShaHeader :: (Functor io, MonadIO io, Str s) 
                    => S3Builder s io ()
addContentShaHeader = 
  hexHash <$> gets s3Body >>= addHeader "x-amz-content-sha256"

-- | Adds a header to the exclusion set for this command. The exclusion of 
-- @Content-Type@ header and any headers starting with @x-amz-@ is disallowed.
-- Excluded headers are always stored in lower-case.
excludeHeader :: (Str s, Monad m) => s -> S3Builder s m ()
excludeHeader (lower -> h) = case lower h of
  "content-type" -> can'tExclude
  "host" -> can'tExclude
  h | isPrefixOf "x-amz-" h -> can'tExclude
  h -> modify $ \c -> c {s3ExcludeHeaders = S.insert h $ s3ExcludeHeaders c}
  where can'tExclude = error $ "Can't exclude " <> show h <> " header"

---------------------------------------------------------------------
-- Building request signature
--
-- There are precise rules for how to build sign an AWS request. Various API
-- calls must implement these rules.
---------------------------------------------------------------------
  
instance Canonical S3Command where
  -- For the full rules, from which these implementations are
  -- adapted, see: http://docs.aws.amazon.com/AmazonS3/latest/API/
  -- sig-v4-header-based-auth.html.
  -- By default, we will take all headers present. The @s3ExcludeHeaders@ set 
  -- can be used to specify headers that shouldn't be included in the canonical
  -- headers. As per the AWS documentation, we must have a host header, so this
  -- will be auto-generated if not present.
  canonicalHeaders S3Command{..} = sortByKey $ exclude $ M.toList hdrs where
    defHost = s3Bucket <> "." <> awsHostName (awsConfig s3Connection)
    hdrs = s3Headers `M.union` M.singleton "host" defHost
    exclude = filter (not . \(h, _) -> S.member h s3ExcludeHeaders)

  canonicalRequest cmd = joinLines [mtd, uri, qry, hdrs, ks, body] where
    prepKey = lower ~> encodeUri True
    prepVal = maybe "" $ trim ~> encodeUri True
    encodeKV (k, v) = prepKey k <> "=" <> prepVal v
    qry = joinBy "&" $ fmap encodeKV $ sortByKey $ s3Query cmd
    uri = encodeUri False $ s3Object cmd
    putColon (k, v) = k <> ":" <> trim v
    body = hexHash $ s3Body cmd
    mtd = show $ s3Method cmd
    hdrs = unlines $ fmap putColon $ canonicalHeaders cmd
    ks = joinSemis $ fmap fst $ canonicalHeaders cmd

-- | Implementation of AWS's rules for constructing the string to sign.
stringToSign :: (Functor io, MonadIO io, Canonical aws, Str s) => aws s -> io s
stringToSign aws = joinLines <$> do
  stamp <- timeFmatLong
  date <- timeFmatShort
  let scope = joinSlashes [date, getRegion aws, getService aws, "aws4_request"]
      request = canonicalRequest aws
  return ["AWS4-HMAC-SHA256", stamp, scope, hexHash request]
  
-- | Generates a one-time secret key according to the Version 4 rules.    
v4Key :: (Functor io, MonadIO io, Aws aws, Str s) => aws s -> io ByteString
v4Key (getCon -> AwsConnection{..}) = do
  date <- timeFmatShort
  return $ foldl1 hmac256 $ map toByteString 
    ["AWS4" <> awsSecretKey awsCredentials, date, awsRegion awsConfig
    , awsService awsConfig, "aws4_request"]

-- | Computes a signature according to the Version 4 rules.
v4Signature :: (Functor io, MonadIO io, Canonical aws, Str s) 
            => aws s -> io ByteString
v4Signature aws = do
  key <- v4Key aws
  tosign <- stringToSign aws
  return $ toHex $ hmac256 key $ toByteString tosign

---------------------------------------------------------------------
-- Building AWS Requests
---------------------------------------------------------------------

awsRequest :: (Functor io, MonadIO io, Req aws, Canonical aws, Str s) 
           => aws s -> io Request
awsRequest aws = do
  (dateStr :: ByteString) <- timeFmatHttpDate
  auth <- v4Signature aws
  liftIO $ buildRequest $ do
    http (getMethod aws) $ (toByteString $ getUri aws)
    setHostname (toByteString $ getHost aws) (getPort aws)
    forM_ (getHeaders aws) $ \(k, v) ->
      wrapByteString2 setHeader (cap k) v
    setHeader "Date" dateStr
    setHeader "Authorization" auth
  where cap h | isPrefixOf "x-" h = h
              | otherwise = capitalize h

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
timeFmatShort = currentTimeFmat "20130524" -- "%Y%m%d"

-- | The "long format" string AWS expects
timeFmatLong :: (Str s, MonadIO io) => io s
timeFmatLong = currentTimeFmat "20130524T000000Z" -- "%Y%m%dT000000Z"

-- | The time format for a Date header.
timeFmatHttpDate :: (Str s, MonadIO io) => io s
timeFmatHttpDate = currentTimeFmat "Fri, 24 May 2013 00:00:00 GMT"

-- | Joins two URI strings together.
joinUri :: Str s => s -> s -> s
joinUri = (<>)

-- | Hashes with HMAC, using the given secret key.
hmac256 :: ByteString -> ByteString -> ByteString
hmac256 key msg = toBytes $ hmacAlg SHA256 key msg

-- | Hashes with SHA256, and then encodes in Base16.
hexHash :: Str s => s -> s
hexHash = asByteString $ B16.encode . SHA256.hash

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
charToHex = fromByteString . upper . cons '%' . B16.encode . singleton

---------------------------------------------------------------------
-- Tests (for debugging)
---------------------------------------------------------------------

-- PART 1: using examples from 
-- http://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-header-based-auth.html

testSecret :: ByteString
testSecret = "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"

testKeyId :: ByteString
testKeyId = "AKIAIOSFODNN7EXAMPLE"

testConnection :: IO (AwsConnection ByteString)
testConnection = createConnection config where
  creds = AwsCredentials testKeyId testSecret
  config = defaultConfig {awsGivenCredentials = Directly creds}

testCommand :: IO (S3Command ByteString)
testCommand = do 
  con <- testConnection
  buildCommand con $ do
    setMethod GET
    addHeader "range" "bytes=0-9"
    addAmzDateHeader
    addContentShaHeader
    setBucket "examplebucket"
    setObject "test.txt"

correctCanonRequest :: ByteString
correctCanonRequest = joinLines 
  [ "GET"
  , "/test.txt"
  , ""
  , "host:examplebucket.s3.amazonaws.com"
  , "range:bytes=0-9"
  , "x-amz-content-sha256:e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
  , "x-amz-date:20130524T000000Z"
  , ""
  , "host;range;x-amz-content-sha256;x-amz-date"
  , "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855" ]
  
correctStringToSign :: ByteString
correctStringToSign = joinLines
  [ "AWS4-HMAC-SHA256"
  , "20130524T000000Z"
  , "20130524/us-east-1/s3/aws4_request"
  , "7344ae5b7ee6c3e7e6b0fe0640412a37625d1fbfff95c48bbb2dc43964946972"
  ]
  
correctSignature :: ByteString
correctSignature = 
  "f0e8bdb87c964420e857bd35b5d6ed310bd44f0170aba48dd91039c6036bdb41"

correctRequest :: IO Request
correctRequest = buildRequest $ do
  http GET "/test.txt"
  setHostname "examplebucket.s3.amazonaws.com" 80
  setHeader "Authorization" correctSignature
  setHeader "Range" "bytes=0-9"
  setHeader "Date" "Fri, 24 May 2013 00:00:00 GMT"
  setHeader "x-amz-content-sha256" emptyStrSha
  setHeader "x-amz-date" "20130524T000000Z"
  where 
    emptyStrSha =
      "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"

assertEqual :: (Str a) => a -> a -> P.String -> IO ()
assertEqual a b msg = 
  if a /= b 
    then do
      putStrLn $ "Assertion failed: not equal (" <> msg <> ")"
      putStrLn a
      putStrLn ("\nDoes not equal\n" :: P.String)
      putStrLn b
    else
      putStrLn ("Assertion succeeded: (" <> msg <> ")")

runTest :: IO ()
runTest = do
  cmd <- testCommand
  let creq = canonicalRequest cmd
  assertEqual creq correctCanonRequest "canonical request"
  s2s <- stringToSign cmd
  assertEqual s2s correctStringToSign "string to sign"
  sig <- v4Signature cmd
  assertEqual sig correctSignature "signature"
  req <- awsRequest cmd
  corReq <- correctRequest
  assertEqual (show req :: ByteString) (show corReq) "request"


-- PART 2: using examples from
-- http://docs.aws.amazon.com/general/latest/gr/sigv4-calculate-signature.html

--testSecret2 :: ByteString
--testSecret2 = "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"

--correctStringToSign :: ByteString
--correctStringToSign = joinLines 
--  [ "AWS4-HMAC-SHA256" , "20110909T233600Z"
--  , "20110909/us-east-1/iam/aws4_request"
--  , "3511de7e95d28ecd39e9513b642aee07e54f4941150d8df8bf94b328ef7e55e2"]

--badDerivedKey :: ByteString
--badDerivedKey = foldl1 hmac256 
--  ["AWS4" <> testSecret, "20130524", "us-east-1", "s3", "aws4_request"]

--testDerivedKey :: ByteString
--testDerivedKey = foldl1 hmac256
--  ["AWS4" <> testSecret2, "20110909", "us-east-1", "iam", "aws4_request"]

--testSignedString :: ByteString
--testSignedString = B16.encode $ hmac256 testDerivedKey correctStringToSign

--correctOctets :: [Octet]
--correctOctets = [152, 241, 216, 137, 254, 196, 244, 66, 26, 220, 82, 43, 171, 12, 225, 248, 46, 105, 41, 194, 98, 237, 21, 229, 169, 76, 144, 239, 209, 227, 176, 231]

--correctString :: ByteString
--correctString = B.pack correctOctets

--testCorrectString :: ByteString
--testCorrectString = B16.encode $ hmac256 correctString correctStringToSign
