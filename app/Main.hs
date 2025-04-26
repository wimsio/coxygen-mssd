module Main where

import Token
import User
import Exchange
import LP

main :: IO ()
main = do
  let token = createToken "SUNDAE" 10000
      user = createUser 1 "Alice"
      listing = listToken token 2.5
      lpTokens = generateMLTTokens ["Alice", "Bob"]
  putStrLn $ "Token: " ++ show token
  putStrLn $ "User: " ++ show user
  putStrLn $ "Listing: " ++ show listing
  putStrLn $ "Liquidity tokens: " ++ show lpTokens