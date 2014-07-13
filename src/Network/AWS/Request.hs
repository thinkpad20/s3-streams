{-# LANGUAGE OverloadedStrings,
             RecordWildCards,
             LambdaCase,
             ViewPatterns,
             ScopedTypeVariables,
             NoMonomorphismRestriction #-}
module Network.AWS.Request (
    module Blaze.ByteString.Builder
  , module Network.Http.Client
  , module System.IO.Streams
  , Req(..)
  , constructRequest, performRequest
  ) where

import qualified Prelude as P
import Blaze.ByteString.Builder (Builder)
import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams
import Network.Http.Client hiding (getHeaders)
import Network.AWS.Core
import Network.AWS.Connection (Canonical(..), getSecurity, getKeyId)
import Network.AWS.Signature (v4Signature, v4Scope)

-- | Class of things that can be made into requests.
class Req a where
  getMethod :: Str s => a s -> Method
  getHeaders :: Str s => a s -> [(s, s)]
  getHost :: Str s => a s -> s
  getPort :: Str s => a s -> Port
  getUri :: Str s => a s -> s
  getBody :: Str s => a s -> ByteString

---------------------------------------------------------------------
-- Building AWS Requests
---------------------------------------------------------------------

-- | AWS requests need to generate an authorization string. AWS4-HMAC-SHA256 Credential=AKIAIOSFODNN7EXAMPLE/20130524/us-east-1/s3/aws4_request,SignedHeaders=host;range;x-amz-content-sha256;x-amz-date,Signature=f0e8bdb87c964420e857bd35b5d6ed310bd44f0170aba48dd91039c6036bdb41
v4AuthString :: (Functor io, MonadIO io, Canonical aws, Req aws, Str s)
             => aws s -> io ByteString
v4AuthString aws = do
  sig <- v4Signature aws
  scope <- v4Scope aws
  let credential = toByteString $ joinSlashes [getKeyId aws, scope]
      sndHeaders = toByteString $ joinSemis $ map fst $ canonicalHeaders aws
  return $ joinCommas
    [ "AWS4-HMAC-SHA256 Credential=" <> credential
    , "SignedHeaders=" <> sndHeaders
    , "Signature=" <> sig]

constructRequest :: (Functor io, MonadIO io, Req req, Canonical req, Str s) 
                 => req s -> io Request
constructRequest req = do
  (dateStr :: ByteString) <- timeFmatHttpDate
  auth <- v4AuthString req
  liftIO $ buildRequest $ do
    http (getMethod req) $ (toByteString $ getUri req)
    setHostname (toByteString $ getHost req) (getPort req)
    forM_ (getHeaders req) $ \(k, v) ->
      wrapByteString2 setHeader (cap k) v
    setHeader "Date" dateStr
    setHeader "Authorization" auth
    when (length (getBody req) > 0) $
      setContentLength $ P.fromIntegral $ length $ getBody req
  where cap h | isPrefixOf "x-" h = h
              | otherwise = capitalize h

---------------------------------------------------------------------
-- Making AWS Requests
---------------------------------------------------------------------

performRequest :: (Req aws, Canonical aws, Str s)
            => (Response -> InputStream ByteString -> IO a)
            -> aws s
            -> IO a
performRequest handler aws = do
  let (host, port) = (toByteString $ getHost aws, getPort aws)
      mkConnection = case getSecurity aws of
        True -> do
          ctx <- baselineContextSSL
          openConnectionSSL ctx host port
        False -> openConnection host port
  req <- constructRequest aws
  withConnection mkConnection $ \con -> do
    body <- Streams.fromByteString $ getBody aws
    case getMethod aws of
      GET -> sendRequest con req emptyBody
      PUT -> sendRequest con req $ inputStreamBody body
      m -> error $ "HTTP method '" <> show m <> "' not yet supported"
    receiveResponse con handler
