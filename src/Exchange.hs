{-@ LIQUID "--no-termination" @-}
module Exchange (Listing(..), listToken) where

import Token

-- Refine: price must be strictly greater than 0
{-@ data Listing = Listing
      { listedToken :: Token
      , price       :: {v:Double | v > 0.0}
      }
  @-}
data Listing = Listing { listedToken :: Token, price :: Double } deriving (Show, Eq)

-- Function refinement: prevent zero or negative price listings
{-@ listToken :: Token -> {v:Double | v > 0.0} -> Listing @-}
listToken :: Token -> Double -> Listing
listToken = Listing

