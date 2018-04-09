# HaskellWorkshop

## How to run the server locally

```
cabal build
cabal run
```

Finally, fire up [localhost:8080](http://localhost:8080).

## How to run the server with Docker

```
docker build -t haskell-workshop .
docker run -it -p 8080:8080 haskell-workshop
```

Finally, fire up [localhost:8080](http://localhost:8080).
