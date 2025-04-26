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

## API Endpoints

### Authentication

All write operations require authentication via the `x-jwt-payload` header. Note that the `x-jwt-payload` header is a decoded JWT and not an actual JWT, since this app is supposed to be deployed behind a front-proxy.

Example JWT payload format for a buyer/seller:
```json
{
  "sub": "a1",
  "name": "Test User",
  "u_typ": "0"
}
```

Example JWT payload format for support:
```json
{
  "sub": "s1",
  "u_typ": "1"
}
```

The JWT payload should be Base64 encoded when sent in the header.

### Endpoints

- `GET /auctions` - List all auctions
- `GET /auction/:id` - Get auction details, including bids and winner information if available
- `POST /auction` - Create a new auction
- `POST /auction/:id/bid` - Place a bid on an auction

### Example Requests

#### Create an auction

```bash
curl -X POST http://localhost:8080/auction \
  -H "Content-Type: application/json" \
  -H "x-jwt-payload: eyJzdWIiOiJhMSIsICJuYW1lIjoiVGVzdCIsICJ1X3R5cCI6IjAifQo=" \
  -d '{
    "id": 1,
    "startsAt": "2023-01-01T10:00:00.000Z",
    "endsAt": "2023-12-31T10:00:00.000Z",
    "title": "Test Auction",
    "currency": "VAC"
  }'
```

#### Place a bid

```bash
curl -X POST http://localhost:8080/auction/1/bid \
  -H "Content-Type: application/json" \
  -H "x-jwt-payload: eyJzdWIiOiJhMiIsICJuYW1lIjoiQnV5ZXIiLCAidV90eXAiOiIwIn0K=" \
  -d '{
    "amount": 100
  }'
```

