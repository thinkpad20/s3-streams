{-# LANGUAGE OverloadedStrings,
             RecordWildCards,
             LambdaCase,
             ViewPatterns,
             ScopedTypeVariables,
             NoMonomorphismRestriction #-}
module Network.AWS.Connection where

import qualified Prelude as P
import qualified Data.List as L
import System.Environment (getEnvironment)
import Network.AWS.Core

---------------------------------------------------------------------
-- AWS Type Classes
-- Abstracts some common capabilities of AWS-related types.
---------------------------------------------------------------------

-- | Class of things which contain AWS configuration.
class AwsCfg a where
  getCon :: Str s => a s -> AwsConnection s

-- | The class of types from which we can generate CanonicalHeaders.
class AwsCfg a => Canonical a where 
  -- | Builds the canonical list of headers.
  canonicalHeaders :: Str s => a s -> [(s, s)]
  -- | Munges information from the command into a "canonical request".
  canonicalRequest :: Str s => a s -> s

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

instance AwsCfg AwsConnection where
  getCon = id

-- | Various getters for AwsCfg types:
getCreds :: (AwsCfg aws, Str s) => aws s -> AwsCredentials s
getCreds = awsCredentials . getCon
getKeyId :: (AwsCfg aws, Str s) => aws s -> s
getKeyId = awsKeyId . getCreds
getSecretKey :: (AwsCfg aws, Str s) => aws s -> s
getSecretKey = awsSecretKey . getCreds
getConfig :: (AwsCfg aws, Str s) => aws s -> AwsConfig s
getConfig = awsConfig . getCon
getHostName :: (AwsCfg aws, Str s) => aws s -> s
getHostName = awsHostName . getConfig
getRegion :: (AwsCfg aws, Str s) => aws s -> s
getRegion = awsRegion . getConfig
getSecurity :: (AwsCfg aws, Str s) => aws s -> Bool
getSecurity = awsIsSecure . getConfig
getService :: (AwsCfg aws, Str s) => aws s -> s
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
                          , awsIsSecure = False
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
-- AWS Monad
---------------------------------------------------------------------

-- | Many operations take place in the context of some AWS connection. We can
-- put them in a monad transformer to simplify them.
type AwsT s = ReaderT (AwsConnection s)

-- | Runs a series of actions in the context of a single connection.
withConnection :: (Str s, MonadIO io, Functor io) 
               => AwsConnection s -> AwsT s io a -> io a
withConnection con = flip runReaderT con

withDefaultConnection :: (Str s, MonadIO io, Functor io) => AwsT s io a -> io a
withDefaultConnection actions = do
  con <- defaultConnection
  withConnection con actions
