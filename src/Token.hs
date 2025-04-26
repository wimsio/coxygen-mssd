{-@ LIQUID "--no-termination" @-}
module Token (Token(..), createToken) where

-- Refined type: quantity must be strictly positive
{-@ data Token = Token
      { tokenName :: String
      , quantity  :: {v:Integer | v > 0}
      }
  @-}
data Token = Token { tokenName :: String, quantity :: Integer } deriving (Show, Eq)

-- Function refinement: only allows positive quantity input
{-@ createToken :: name:String -> {qty:Integer | qty > 0} -> Token @-}
createToken :: String -> Integer -> Token
createToken = Token

