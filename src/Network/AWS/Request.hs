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
import Network.AWS.Connection (Canonical(..), getSecurity)
import Network.AWS.Signature (v4Signature)

-- | Class of things that can be made into requests.
class Req a where
  getMethod :: Str s => a s -> Method
  getHeaders :: Str s => a s -> [(s, s)]
  getHost :: Str s => a s -> s
  getPort :: Str s => a s -> Port
  getUri :: Str s => a s -> s

---------------------------------------------------------------------
-- Building AWS Requests
---------------------------------------------------------------------
  
constructRequest :: (Functor io, MonadIO io, Req aws, Canonical aws, Str s) 
                 => aws s -> io Request
constructRequest aws = do
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
-- Making AWS Requests
---------------------------------------------------------------------

openConnection' :: Bool -> Hostname -> Port -> IO Connection
openConnection' False h p = openConnection h p
openConnection' True h p = do
  ctx <- baselineContextSSL
  openConnectionSSL ctx h p

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
    case getMethod aws of
      GET -> sendRequest con req emptyBody
      m -> error $ "HTTP method '" <> show m <> "' not yet supported"
    receiveResponse con handler
