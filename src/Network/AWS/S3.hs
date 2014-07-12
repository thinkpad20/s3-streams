{-# LANGUAGE OverloadedStrings,
             RecordWildCards,
             LambdaCase,
             ViewPatterns,
             ScopedTypeVariables,
             NoMonomorphismRestriction #-}
module Network.AWS.S3 where

import qualified Prelude as P
import Network.AWS.Core
import Network.AWS.Connection
import Network.AWS.Request
import qualified Data.HashMap.Strict as M
import qualified Data.Set as S

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
  
instance Aws S3Command where
  getCon = s3Connection
  
instance Req S3Command where
  getMethod = s3Method
  getHeaders S3Command{..} = M.toList s3Headers
  getHost cmd = s3Bucket cmd <> "." <> getHostName cmd
  getPort _ = 80
  getUri S3Command{..} = addSlash s3Object

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
-- Common S3 Commands
---------------------------------------------------------------------

-- | An S3 GET command, given a bucket and an object.
s3GetCmd :: (Str s, Monad m) => s -> s -> AwsConnection s -> m (S3Command s)
s3GetCmd bucket object con = buildCommand con $ do
  setBucket bucket
  setObject object
  
-- | Takes a handler for the response, and GETs a bucket/object.
s3Get :: Str s => s -> s 
      -> AwsConnection s
      -> (Response -> InputStream ByteString -> IO a)
      -> IO a
s3Get bucket object con handler =
  s3GetCmd bucket object con >>= performRequest handler
  
