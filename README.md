# S3-Streams

#### An Amazon S3 client in Haskell, based on Http-Streams

This is an implementation of the AWS S3 API using `http-streams`, an HTTP client written using `io-streams`, a relatively lightweight and simple Haskell IO manager with strict guarantees. The existing `aws` package is large and full-featured, but it relies on Conduits, bringing in a large dependency as well as cognitive overhead for those who don't want to learn a rather complex API. There is also the `hS3` package, which is much simpler but uses vanilla IO and doesn't provide guarantees of constant space. The goal of this is to provide a middle-ground: a lean and lightweight, but usable and powerful S3 client.

### Example:

```haskell
import Network.AWS.S3
import Network.Http.Client

main = do
  con <- defaultConnection
  s3Get "my_bucket" "my_object" con debugHandler
```

### Status:

Still in development and not yet usable.

### License: 

MIT
