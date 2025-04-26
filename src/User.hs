{-@ LIQUID "--no-termination" @-}
module User (User(..), createUser) where

import Token

-- User ID must be non-negative, and holdings list must be empty when created
{-@ data User = User
      { userId   :: {v:Int | v >= 0}
      , username :: String
      , holdings :: [Token]
      }
  @-}
data User = User { userId :: Int, username :: String, holdings :: [Token] } deriving (Show, Eq)

-- Guarantees initial holdings are empty and userId is non-negative
{-@ createUser :: {v:Int | v >= 0} -> String -> User @-}
createUser :: Int -> String -> User
createUser uid uname = User uid uname []

