{-# LANGUAGE OverloadedStrings,
             RecordWildCards,
             LambdaCase,
             ViewPatterns,
             ScopedTypeVariables,
             NoMonomorphismRestriction #-}
module Network.AWS.S3.Object where

import qualified Prelude as P
import Network.AWS.Core

---------------------------------------------------------------------
-- S3 Object Commands
---------------------------------------------------------------------

-- -- | An S3 GET command, given a bucket and an object.
-- getObject :: (Str s, Functor io, MonadIO io) 
--           => s -> s -> AwsConnection s -> io (S3Command s)
-- getObject bucket object con = 
--   cmd buildCommand con $ do
--     setBucket bucket
--     setObject object
-- 
-- putFile :: (Str s, Functor io, MonadIO io)
--         => s -> s -> s -> AwsT s io ()
-- putFile bucket object path = do
--   cmd <- buildCommandM $ do
--     setBucket bucket
--     setObject object
--     setMethod PUT
--     setStorageClass STANDARD
--   performRequest cmd 
--   
