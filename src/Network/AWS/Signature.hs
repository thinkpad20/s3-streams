{-# LANGUAGE OverloadedStrings,
             RecordWildCards,
             LambdaCase,
             ViewPatterns,
             ScopedTypeVariables,
             NoMonomorphismRestriction #-}
module Network.AWS.Signature where

import qualified Prelude as P
import Network.AWS.Core
import Network.AWS.Connection ( Aws(..), Canonical(..), AwsConnection(..)
                              , getRegion, getService, getSecretKey)

---------------------------------------------------------------------
-- Building request signature
--
-- There are precise rules for how to build sign an AWS request. Various API
-- calls must implement these rules.
---------------------------------------------------------------------

-- | Generating a scope string, used in the StringToSign and the Authorization
-- string.
v4Scope :: (Functor io, MonadIO io, Canonical aws, Str s) => aws s -> io s
v4Scope aws = do 
  date <- timeFmatShort
  return $ joinSlashes [date, getRegion aws, getService aws, "aws4_request"]

-- | Implementation of AWS's rules for constructing the string to sign.
stringToSign :: (Functor io, MonadIO io, Canonical aws, Str s) => aws s -> io s
stringToSign aws = joinLines <$> do
  stamp <- timeFmatLong
  scope <- v4Scope aws
  return ["AWS4-HMAC-SHA256", stamp, scope, hexHash $ canonicalRequest aws]
  
-- | Generates a one-time secret key according to the Version 4 rules.    
v4Key :: (Functor io, MonadIO io, Aws aws, Str s) => aws s -> io ByteString
v4Key aws = do
  date <- timeFmatShort
  return $ foldl1 hmac256 $ map toByteString 
    ["AWS4" <> getSecretKey aws, date, getRegion aws
    , getService aws, "aws4_request"]

-- | Computes a signature according to the Version 4 rules.
v4Signature :: (Functor io, MonadIO io, Canonical aws, Str s) 
            => aws s -> io ByteString
v4Signature aws = do
  key <- v4Key aws
  tosign <- stringToSign aws
  return $ toHex $ hmac256 key $ toByteString tosign
