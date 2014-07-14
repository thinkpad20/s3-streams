# S3-Streams

#### An Amazon S3 client in Haskell, based on Http-Streams

This is an implementation of the AWS S3 API using `http-streams`, an HTTP client written using `io-streams`, a relatively lightweight and simple Haskell IO manager with strict guarantees. The existing `aws` package is large and full-featured, but it relies on Conduits, bringing in a large dependency as well as cognitive overhead for those who don't want to learn a rather complex API. There is also the `hS3` package, which is much simpler but uses vanilla IO and doesn't provide guarantees of constant space. The goal of this is to provide a middle-ground: a lean and lightweight, but usable and powerful S3 client.

### Example:

First, load your AWS access key id and secret access key into your environment under the keys `AWS_ACCESS_KEY_ID` and `AWS_SECRET_ACCESS_KEY`, respectively. Then, assuming you have the bucket `my_bucket` with object `my_object`, this code will print the contents of `my_object` to stdout:

```haskell
import qualified Network.AWS.S3 as S3
import Network.Http.Client (debugHandler)
import System.IO.Streams as Streams

main = do
  -- Build a connection with default options
  con <- S3.defaultConnection
  cmd <- S3.get "my_bucket" "my_object" con
  -- Prints the contents of `my_object` to stdout.
  S3.performRequest debugHandler cmd
```

We can also write to an S3 bucket; for example:

```haskell
main = do
  con <- S3.defaultConnection
  cmd <- S3.buildCommand' con $ do
    setMethod PUT
    setBucket "my_bucket"
    setObject "my_object"
    setBody "Hello, world!"
  S3.performRequest echoHandler cmd
```

### Status:

Just recently passed the milestone of making a successful GET request. Still quite a ways to go but (it seems) the worst is passed.

### License: 

MIT
