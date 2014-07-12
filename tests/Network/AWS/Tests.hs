{-# LANGUAGE OverloadedStrings,
             RecordWildCards,
             LambdaCase,
             ViewPatterns,
             ScopedTypeVariables,
             NoMonomorphismRestriction #-}
module Network.AWS.Tests where

import qualified Prelude as P
import Network.AWS.Core
import Network.AWS.Connection
import Network.AWS.Request
import Network.AWS.Signature
import Network.AWS.S3


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
