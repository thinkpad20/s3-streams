{-# LANGUAGE OverloadedStrings,
             RecordWildCards,
             LambdaCase,
             ViewPatterns,
             ScopedTypeVariables,
             NoMonomorphismRestriction #-}
module Network.AWS.Request where

import Network.AWS.Core
import Network.AWS.Connection (Canonical(..))
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
