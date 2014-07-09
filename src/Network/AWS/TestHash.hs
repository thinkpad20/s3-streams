{-# LANGUAGE OverloadedStrings #-}
module Network.AWS.TestHash where

import Data.Byteable (toBytes)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import Data.List (intercalate)
import Data.Monoid
import Crypto.Hash
import Codec.Utils (Octet)
import qualified Crypto.Hash.SHA256 as SHA256

testSecret :: ByteString
testSecret = "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"

testStringToSign :: ByteString
testStringToSign = B.intercalate "\n" 
  [ "AWS4-HMAC-SHA256" , "20110909T233600Z"
  , "20110909/us-east-1/iam/aws4_request"
  , "3511de7e95d28ecd39e9513b642aee07e54f4941150d8df8bf94b328ef7e55e2"]

testDerivedKey :: ByteString
testDerivedKey = foldl1 hmac256
  ["AWS4" <> testSecret, "20110909", "us-east-1", "iam", "aws4_request"]

testSignedString :: ByteString
testSignedString = hmac256 testDerivedKey testStringToSign

correctOctets :: [Octet]
correctOctets = [152, 241, 216, 137, 254, 196, 244, 66, 26, 220, 82, 43, 171, 12, 225, 248, 46, 105, 41, 194, 98, 237, 21, 229, 169, 76, 144, 239, 209, 227, 176, 231]

correctString :: ByteString
correctString = B.pack correctOctets

testCorrectString :: ByteString
testCorrectString = B16.encode $ hmac256 correctString testStringToSign

-- | Hashes with HMAC, using the given secret key.
hmac256 :: ByteString -> ByteString -> ByteString
hmac256 key msg = toBytes $ hmacAlg SHA256 key msg

main = do
  let octets = B.unpack testDerivedKey
  putStrLn $ intercalate " " $ map show octets
