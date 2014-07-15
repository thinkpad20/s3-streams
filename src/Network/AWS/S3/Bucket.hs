{-# LANGUAGE OverloadedStrings,
             RecordWildCards,
             LambdaCase,
             ViewPatterns,
             ScopedTypeVariables,
             NoMonomorphismRestriction #-}
module Network.AWS.S3.Bucket where

import qualified Prelude as P
import Network.AWS.Core
import Network.AWS.Connection
import Network.AWS.S3.Command

-- | Gets the files in a bucket.
getBucket :: (Str s, Functor io, MonadIO io)
          => s -> AwsConnection s -> io (S3Command s)
getBucket bucket con = buildCommand con $ setBucket bucket
