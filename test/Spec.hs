module Main where

import Test.QuickCheck
import Token
import User
import Exchange
import LP
import Data.List (nub)

instance Arbitrary Token where
  arbitrary = do
    name <- elements ["SUNDAE", "MILKSHAKE", "CONE", "SPRINKLES"]
    Positive qty <- arbitrary
    return (createToken name qty)

instance Arbitrary User where
  arbitrary = do
    uid <- arbitrary
    uname <- elements ["Alice", "Bob", "Charlie"]
    return (createUser uid uname)

instance Arbitrary LPToken where
  arbitrary = LPToken <$> elements ["Alice", "Bob", "Charlie"] <*> pure 100

prop_createToken :: String -> Positive Integer -> Bool
prop_createToken name (Positive qty) =
  let token = createToken name qty
  in tokenName token == name && quantity token == qty

prop_createUser :: Int -> String -> Bool
prop_createUser uid uname =
  let user = createUser uid uname
  in userId user == uid && username user == uname && null (holdings user)

prop_listingPricePositive :: Token -> Positive Double -> Bool
prop_listingPricePositive token (Positive price) =
  let listing = listToken token price
  in price > 0 && listedToken listing == token

prop_generateMLTTokens :: [String] -> Bool
prop_generateMLTTokens owners =
  let lpTokens = generateMLTTokens owners
  in length lpTokens == length owners &&
     all (\lp -> lpQty lp == 100) lpTokens &&
     nub (map lpOwner lpTokens) == nub owners

main :: IO ()
main = do
  quickCheck prop_createToken
  quickCheck prop_createUser
  quickCheck prop_listingPricePositive
  quickCheck prop_generateMLTTokens