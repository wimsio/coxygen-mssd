{-@ LIQUID "--no-termination" @-}
module LP (LPToken(..), generateMLTTokens) where

{-@ data LPToken = LPToken
      { lpOwner :: String
      , lpQty   :: {v:Integer | v == 100}
      }
  @-}
data LPToken = LPToken { lpOwner :: String, lpQty :: Integer } deriving (Show, Eq)

{-@ generateMLTTokens :: [String] -> [{v:LPToken | lpQty v == 100}] @-}
generateMLTTokens :: [String] -> [LPToken]
generateMLTTokens owners = [ LPToken owner 100 | owner <- owners ]

