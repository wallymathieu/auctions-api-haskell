module Domain (TimedAscendingOptions, SingleSealedBidOptions,
               AuctionType, Auction) where

import Money
import Data.List
type UserId = String

type BidId = String

type AuctionId = Integer

-- milliseconds to begin with
type TimeSpan = Integer 
-- milliseconds to begin with
type DateTime = Integer

data TimedAscendingOptions = TimedAscendingOptions { 
      {- the seller has set a minimum sale price in advance (the 'reserve' price) 
       and the final bid does not reach that price the item remains unsold
       If the reserve price is 0, that is the equivalent of not setting it. -}
      reservePrice:: Amount,
      {- Sometimes the auctioneer sets a minimum amount by which the next bid must exceed the current highest bid.
       Having min raise equal to 0 is the equivalent of not setting it.-}
      minRaise:: Amount,
      {- If no competing bidder challenges the standing bid within a given time frame, 
       the standing bid becomes the winner, and the item is sold to the highest bidder 
       at a price equal to his or her bid. -}
      timeFrame:: TimeSpan
    }

data SingleSealedBidOptions =
    {- Sealed first-price auction 
     In this type of auction all bidders simultaneously submit sealed bids so that no bidder knows the bid of any
     other participant. The highest bidder pays the price they submitted.
     This type of auction is distinct from the English auction, in that bidders can only submit one bid each.-}
    Blind
    {- Also known as a sealed-bid second-price auction.
     This is identical to the sealed first-price auction except that the winning bidder pays the second-highest bid
     rather than his or her own -}
    |Vickrey

data AuctionType=
     {- also known as an open ascending price auction
      The auction ends when no participant is willing to bid further -}
     TimedAscending TimedAscendingOptions
     | SingleSealedBid SingleSealedBidOptions

data Auction = Auction { auctionId :: AuctionId,
    startsAt :: DateTime,
    title :: String,
    -- initial expiry
    expiry :: DateTime,
    seller :: UserId,
    typ :: AuctionType,
    currency :: Currency
  }

data Bid = Bid { bidId :: BidId,
  auction :: AuctionId,
  bidder :: UserId,
  at :: DateTime,
  amount :: Amount
}

data TimedAscendingState =
    AwaitingStart { start:: DateTime , startingExpiry:: DateTime , opt::TimedAscendingOptions }
    | OnGoing { bids:: [Bid] , nextExpiry:: DateTime , opt::TimedAscendingOptions }
    | HasEnded { bids:: [Bid] , expired:: DateTime , opt::TimedAscendingOptions }
