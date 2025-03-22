# auction-site-haskell

Auctions engine implemented in haskell (mostly intended as POC) 

## Variants

- [Spock](https://github.com/wallymathieu/auctions-api-haskell/tree/spock)
- [Scotty](https://github.com/wallymathieu/auctions-api-haskell/tree/scotty)
- [Servant](https://github.com/wallymathieu/auctions-api-haskell/tree/servant)

## To Develop

Install GHC using [ghcup](https://www.haskell.org/ghcup/)

```sh
ghcup install ghc 9.4.8
ghcup set ghc 9.4.8
```

### To run the app

```sh
cabal run
```

### To test the app

```sh
cabal test
```

### To lint the app

```sh
cabal install hlint
hlint .
```