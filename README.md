
# 🍨 MSSD — Mockup Sundae Swap DEX (Type-Safe Haskell Project)

![image](https://github.com/user-attachments/assets/0a260d32-1c4c-4df1-9594-a201529f6976)

![Pasted image (16)](https://github.com/user-attachments/assets/c96f4a17-0a09-4ed3-b42a-95fb9799d119)

Welcome to the **MSSD** project — a beginner-friendly, strongly typed mock decentralized exchange built in **pure Haskell**. This project uses GADTs, smart constructors, and property-based testing to teach you core functional programming concepts and formal correctness.

Coxygen Global aims to the global leader in Cardano students onboardment. This project named MSSD is one of those Real Life Use Case Based Practicals [RLUCBP]. This involves modelling a real life
situation coding it in Haskell/Plutus etc in order to gain skills of coding and knowledge about projects development. Aim is to speed up development at the same time adhere to coding standards with new developers and students.

---

## 📁 Project Structure

```
MSSD/
├── app/Main.hs           -- Entry point
├── src/
│   ├── Token.hs          -- Tokens with quantity > 0
│   ├── User.hs           -- Users with non-negative IDs
│   ├── Exchange.hs       -- Listings with price > 0
│   └── LP.hs             -- Liquidity providers (fixed amount)
├── test/Spec.hs          -- Property-based tests
├── MSSD.cabal            -- Project config for Cabal
└── README.md             -- This file
```

---
## git for new users Linux/WSL terminal(cmd.exe)

```
- install git
- git init - this will initilize a git configuration file
- git branch -m <branch name : This is a string so use "dev" or "main"> - creates a new branch with name given

### - Login into github and create a new empty repository. Copy its git...url from the green button

- git add . - this will add all files in the local project to the stagging area, an area before a commit area
- git commit -m <message for committing this project as a string>
- git remote add origin <url - e.g. https://github.com/wimsio/coxygen-mssd.git> if there are remote and local files conflicts then resolve them first
- git pull origin main --allow-unrelated-histories - This will pull the remote project download it to local and allow unrelated files to exist
- git branch -M main
- git push -u origin main - If there are conflicts again try git config pull.rebase false 
- git push origin main - again should work. 

  If you see something like code below Congratulations! then check your remote project files have been uploaded. If you see nothing then reload the web page.
  Enumerating objects: 82, done.
  Counting objects: 100% (82/82), done.
  Delta compression using up to 12 threads
  Compressing objects: 100% (68/68), done.
  Writing objects: 100% (80/80), 2.45 MiB | 1.31 MiB/s, done.
  Total 80 (delta 14), reused 0 (delta 0), pack-reused 0
  remote: Resolving deltas: 100% (14/14), done.
  To https://github.com/wimsio/coxygen-mssd.git
    b88d160..92f650f  main -> main
  branch 'main' set up to track 'origin/main'.

### - Updating local changes to remote project

- git add . - after adding or modifying local project files
- git commit -m "Updated the Readme file by adding git steps for new developers". You should see something like code below:

  [main fcd1eb7] Updated the Readme file by adding git steps for new developers
  1 file changed, 33 insertions(+)

- git push origin main - You should see files uploaded with their sizes e.g. code below

  [main fcd1eb7] Updated the Readme file by adding git steps for new developers
  1 file changed, 33 insertions(+)

```

---

## 🧱 Modules Overview

### 🎫 `Token.hs`
Defines a token with a name and quantity. Enforced invariant: `quantity > 0`.

```haskell
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
```

---

### 👤 `User.hs`
Defines a user with a unique ID, name, and list of tokens. Enforced: `userId >= 0`.

```haskell
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
```

---

### 💱 `Exchange.hs`
Defines a listing for a token with a price. Enforced: `price > 0`.

```haskell
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


```

---

### 💧 `LP.hs`
Liquidity provider tokens with fixed quantity of 100.

```haskell
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


```
### 💧 `app/Main.hs`
This is the entry point of the project when running. The main imports other modules Token, User, Exchange and LP and has their functions tested.
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

```haskell

```

---

## 💻 Getting Started

### 🔧 Install GHC and Cabal

Use [GHCup](https://www.haskell.org/ghcup/) to install:

```bash
curl -sSf https://get-ghcup.haskell.org | sh
ghcup install ghc 9.2.8
ghcup set ghc 9.2.8
ghcup install cabal
```

---

## ⚙️ Build the Project with Cabal

```bash
cabal update
cabal build
```

To run:

```bash
cabal run
```

---

## 🧪 Property-Based Testing with QuickCheck & Tasty

### 📦 Install testing dependencies

Make sure your `.cabal` file includes:

```cabal
build-depends:
  base,
  QuickCheck,
  tasty,
  tasty-quickcheck
```

### 📁 Sample `test/Spec.hs`

```haskell
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
```

### ▶️ Run the tests

```bash
cabal test
```

---

## 🧠 Using GHCi (REPL)

### ▶️ Launch REPL

```bash
ghci src/Token.hs
```

### 🧪 Test your functions

```haskell
:t createToken
-- String -> Integer -> Maybe (Token 'Valid')

let t = createToken "SUNDAE" 100
:t t
-- Maybe (Token 'Valid')
```

### 👁 Inspect values

```haskell
case t of
  Just tok -> putStrLn (tokenName tok)
  Nothing  -> putStrLn "Invalid token"
```

---

## 📄 Understanding the `.cabal` File

```cabal
-- File: mockup-sundae-swap-decentralized-exchange.cabal

name:                mssd
version:             0.1.0.0
build-type:          Simple
cabal-version:       >=1.10

library
  hs-source-dirs:      src
  exposed-modules:
      Token
    , User
    , Exchange
    , LP
  build-depends:
      base >=4.7 && <5
  default-language:    Haskell2010

test-suite mssd-test
  type:                exitcode-stdio-1.0
  hs-source-dirs:      test
  main-is:             Spec.hs
  build-depends:
      base
    , mssd
    , tasty
    , tasty-hunit
    , QuickCheck >=2.14
    , tasty-quickcheck
  default-language:    Haskell2010

executable mssd-app
  hs-source-dirs:      app
  main-is:             Main.hs
  build-depends:
      base       >=4.7 && <5
    , mssd
  default-language:    Haskell2010

```

### Key Fields

- `exposed-modules`: modules other files can import
- `hs-source-dirs`: where your `.hs` files are
- `build-depends`: libraries required
- `default-language`: Haskell2010 is safe default

---

## ❤️ Why Use GADTs Instead of LiquidHaskell?

| Feature              | LiquidHaskell      | GADTs & DataKinds     |
|---------------------|--------------------|------------------------|
| External tool needed| ✅ Yes              | ❌ No (GHC only)       |
| Compile-time safety | ✅ Yes              | ✅ Yes                 |
| Error readability   | ❌ Sometimes cryptic| ✅ GHC messages         |
| Setup complexity    | High               | Low                    |

---

## 📚 Want to Learn More?

- [GADTs in Haskell](https://wiki.haskell.org/GADT)
- [QuickCheck Docs](https://hackage.haskell.org/package/QuickCheck)
- [Tasty Test Framework](https://hackage.haskell.org/package/tasty)

---

## 🙌 Final Words

This project is designed to make you **think in types** 🧠 — the more logic you encode in types, the fewer runtime bugs you'll see. If you'd like to extend it with more safe-state transitions (like trades, swaps, vesting), you're on the right track!

Enjoy the MSSD. 🍨
