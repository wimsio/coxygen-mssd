# 🍨 **MSSD — Mockup Sundae Swap DEX**  

<img src="https://github.com/user-attachments/assets/c96f4a17-0a09-4ed3-b42a-95fb9799d119" alt="placeholder" width="50" height="50">

### *[Cabal, Haskell, Property Based Tests (QuickCheck), Logic & Maths Proofing (LiquidHaskell/GADTs)]*

<img src="https://github.com/user-attachments/assets/0a260d32-1c4c-4df1-9594-a201529f6976" alt="placeholder" width="250" height="150">

## 📖 **Table of Contents**
1. [Introduction](#introduction)
2. [Project Structure](#project-structure)
3. [Git Basics for Beginners](#git-basics-for-beginners)
4. [Modules Overview](#modules-overview)
   1. [Token.hs](#tokenhs)
   2. [User.hs](#userhs)
   3. [Exchange.hs](#exchangehs)
   4. [LP.hs](#lphs)
   5. [Main.hs](#mainhs)
5. [Getting Started](#getting-started)
6. [Property-Based Testing](#property-based-testing)
7. [Using GHCi (REPL)](#using-ghci-repl)
8. [Understanding the `.cabal` File](#understanding-the-cabal-file)
9. [GADTs vs LiquidHaskell](#gadts-vs-liquidhaskell)
10. [Additional Resources](#additional-resources)
11. [Final Words](#final-words)

---

## 1. 🎉 **Introduction** <a id="introduction"></a>

Welcome to the **MSSD** project—a beginner-friendly, strongly typed mock decentralized exchange built in **pure Haskell**. This project uses GADTs, smart constructors, and property-based testing to teach core functional programming concepts and formal correctness.

Coxygen Global aims to be a global leader in onboarding Cardano students. MSSD is a Real Life Use Case Based Practical (RLUCBP) designed to model real-life scenarios in Haskell/Plutus to speed up skill development and adhere to coding standards.

New to Decentralized Exchanges? Check out [SundaeSwap](https://v2.sundaeswap.finance).

When you git clone, cabal update, cabal build, cabal run or cabal test this is what you should get 

![image](https://github.com/user-attachments/assets/d1b0cc89-7e39-415d-8f9b-e2d2ed79e576)

---

## 2. 📁 **Project Structure** <a id="project-structure"></a>

```bash
MSSD/
├── app/Main.hs           -- Entry point
├── src/
│   ├── Token.hs          -- Tokens with quantity > 0
│   ├── User.hs           -- Users with non-negative IDs
│   ├── Exchange.hs       -- Listings with price > 0
│   └── LP.hs             -- Liquidity providers (fixed amount)
├── test/Spec.hs          -- Property-based tests
├── MSSD.cabal            -- Project config for Cabal
└── README.md             -- Documentation
```

---

## 3. 🖥️ **Git Basics for Beginners** <a id="git-basics-for-beginners"></a>

```bash
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

## 4. 🧱 **Modules Overview** <a id="modules-overview"></a>

### 4.1 🎫 `Token.hs` <a id="tokenhs"></a>
Defines tokens ensuring quantity > 0.

```haskell
{-@ createToken :: name:String -> {qty:Integer | qty > 0} -> Token @-}

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

### 4.2 👤 `User.hs` <a id="userhs"></a>
Users with non-negative IDs and initially empty token holdings.

```haskell
{-@ createUser :: {v:Int | v >= 0} -> String -> User @-}

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

### 4.3 💱 `Exchange.hs` <a id="exchangehs"></a>
Listings ensuring prices are strictly positive.

```haskell
{-@ listToken :: Token -> {v:Double | v > 0.0} -> Listing @-}

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

### 4.4 💧 `LP.hs` <a id="lphs"></a>
Liquidity provider tokens fixed at quantity 100.

```haskell
{-@ generateMLTTokens :: [String] -> [{v:LPToken | lpQty v == 100}] @-}

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

### 4.5 🚀 `Main.hs` <a id="mainhs"></a>
Demonstrates module integration and function usage.

---

### 4.6 🚀 Reasoning your code on modules User.hs, Token.hs, LP.hs and Exchange.hs. Adding logic and maths to improving your code with formal methods demo
#### a. Token.hs
```haskell
Here’s what each part of your Liquid Haskell–annotated module is doing:

--- LP

## 1. `{-@ LIQUID "--no-termination" @-}`

By default Liquid Haskell tries to verify that **every** function in your module terminates.  
- The flag `--no-termination` disables that check so you don’t need to prove termination for recursive or list-comprehension-based definitions.  
- This is often used during development, or when you know your code terminates but don’t want to write an explicit termination measure.

---

## 2. Refining the `LPToken` data type

```haskell
{-@ data LPToken = LPToken
      { lpOwner :: String
      , lpQty   :: {v:Integer | v == 100}
      }
  @-}


- **`data LPToken = LPToken { … }`** is your usual Haskell ADT.  
- The Liquid Haskell annotation re-declares it with **refined field types**:  
  - `lpOwner :: String` stays the same.  
  - `lpQty   :: {v:Integer | v == 100}` means “the `lpQty` field must always be **exactly** 100.”  
- Liquid Haskell will now reject any code that tries to construct an `LPToken` where `lpQty` is not 100.

---

## 3. Specifying the `generateMLTTokens` function

```haskell
{-@ generateMLTTokens :: [String] -> [{v:LPToken | lpQty v == 100}] @-}
generateMLTTokens :: [String] -> [LPToken]
generateMLTTokens owners = [ LPToken owner 100 | owner <- owners ]


- The annotation says:
  - Input: a list of `String`.  
  - Output: a list of `LPToken`s, each of which must satisfy the predicate `lpQty v == 100`.  
- Liquid Haskell uses this spec to check that the list comprehension `[ LPToken owner 100 | … ]` indeed only produces tokens whose `lpQty` equals 100.  
- If you accidentally wrote something like `LPToken owner 50`, LH would flag a type error.

---

### Why these refinements help

- **Stronger invariants at compile time.** You guarantee that **no** `LPToken` can ever carry a quantity other than 100.  
- **Self-documenting API.** Anyone reading your type signatures immediately knows the intended “fixed-100” semantics.  
- **Automatic checking.** Liquid Haskell enforces the annotation, so you get early feedback if the code drifts.

--

```
#### b. User.hs
```Haskell

{-@ LIQUID "--no-termination" @-}

- Turns off Liquid Haskell’s requirement to prove that every function terminates.
- Handy when you’re using simple definitions (like list comprehensions or single‐clause functions) and don’t want to supply a termination metric.
- You can safely remove this once you add any recursive definitions and provide a decreasing measure.

---

## 2. Refining the `User` Data Type

```haskell
{-@ data User = User
      { userId   :: {v:Int | v >= 0}
      , username :: String
      , holdings :: [Token]
      }
  @-}
data User = User
  { userId   :: Int
  , username :: String
  , holdings :: [Token]
  }
  deriving (Show, Eq)


- **`userId :: {v:Int | v >= 0}`**  
  Enforces that every `User` must have a non-negative ID. Any attempt to construct a `User` with a negative `userId` will be rejected.
- **`username :: String`**  
  Left unrefined, so any string is valid.
- **`holdings :: [Token]`**  
  Lists the tokens a user holds; no extra refinement here, but see the constructor spec below.

---

## 3. Specifying `createUser`

```haskell
{-@ createUser :: {v:Int | v >= 0} -> String -> User @-}
createUser :: Int -> String -> User
createUser uid uname = User uid uname []


- **Input refinements:**  
  - The first argument (`uid`) must be ≥ 0.  
  - The second argument (`uname`) is any `String`.  
- **Output guarantee:**  
  Because the only way to build a `User` via `createUser` is with an empty list for `holdings`, Liquid Haskell knows every `User` created this way starts with no tokens.

---

## 4. Benefits of These Annotations

1. **Strong Invariants**  
   - No `User` can ever slip through with a negative ID.  
   - New users always begin with zero holdings.

2. **Self-Documenting Types**  
   - Reading the signature, you immediately see “IDs are non-negative” and “holdings start empty.”

3. **Compile-Time Safety**  
   - Mistakes like `createUser (-1) "bob"` or manually writing `User 5 "alice" [someToken]` won’t type‐check unless you explicitly bypass the spec.

---

### Next Steps

- **Re-enable termination checks** by removing `--no-termination` once you add recursive functions (e.g., updating holdings).  
- **Add further refinements** if you introduce rules like “username must be non-empty” (`{v:String | len v > 0}`) or “holdings can’t exceed a certain size.”  

With these specs, your user‐management layer is guaranteed to start users in a valid state, enforced by the type system itself.

```
#### c. Exchange.hs

Here’s a breakdown of the Liquid Haskell annotations in your `Exchange` module:

```haskell

## 1. `{-@ LIQUID "--no-termination" @-}`

- Disables Liquid Haskell’s termination checker for this module.  
- Useful when you know your definitions terminate (or you don’t need the check) but haven’t provided an explicit decreasing measure.  
- You can drop this flag later once you’ve proved termination (e.g., by supplying a metric for recursion).

---

## 2. Refining the `Listing` data type


{-@ data Listing = Listing
      { listedToken :: Token
      , price       :: {v:Double | v > 0.0}
      }
  @-}
data Listing = Listing { listedToken :: Token, price :: Double }
  deriving (Show, Eq)

- `listedToken :: Token` remains unconstrained beyond the `Token` type you’ve defined elsewhere.  
- `price :: {v:Double | v > 0.0}` enforces that any `Listing` must carry a strictly positive price.  
- Any attempt to construct `Listing … price = 0.0` or a negative price is rejected at compile time.

## 3. Specifying the `listToken` function

{-@ listToken :: Token -> {v:Double | v > 0.0} -> Listing @-}
listToken :: Token -> Double -> Listing
listToken = Listing

- The type annotation says:
  1. You pass in a `Token`.  
  2. You pass in a `Double` that Liquid Haskell knows must be strictly greater than 0.0.  
  3. You get back a `Listing` whose `price` field is guaranteed to satisfy that same `> 0.0` refinement.  
- This prevents you from even calling `listToken` with a zero or negative price—LH will flag it as a type error before compilation.

---

## 4. Benefits of these refinements

- **Safety:** You can never create a “free” or negative‐priced listing by mistake.  
- **Clarity:** The API’s intent (“price must be positive”) is documented directly in the types.  
- **Early feedback:** Liquid Haskell enforces these invariants at compile time, catching errors immediately.

---

**Next steps:**  
- Once you’ve confirmed termination for any recursive functions you add to this module, consider removing `--no-termination` to re‐enable full termination checking.
- If you need more sophisticated pricing rules (e.g., upper bounds, currency checks), you can refine further (e.g., `{v:Double | v > 0.0 && v < 1e6}`).

```

#### d. LP.hs

Here’s a similar breakdown for your `LP.hs` module with Liquid Haskell refinements:

```Haskell

## 1. Disabling Termination Checking

{-@ LIQUID "--no-termination" @-}

- Turns off Liquid Haskell’s termination checker for this file.
- Useful if you have list comprehensions or recursion and don’t want to supply a termination metric right now.
- You can remove this flag later once you’ve proven (or measured) that all functions terminate.

## 2. Refining the `LPToken` Type

{-@ data LPToken = LPToken
      { lpOwner :: String
      , lpQty   :: {v:Integer | v == 100}
      }
  @-}
data LPToken = LPToken { lpOwner :: String, lpQty :: Integer }
  deriving (Show, Eq)

- **`lpOwner :: String`** remains unrefined—any string is allowed.
- **`lpQty :: {v:Integer | v == 100}`** fixes the token quantity to exactly `100`.  
  - Liquid Haskell will reject any construction where `lpQty /= 100`.

---

## 3. Annotating `generateMLTTokens`

{-@ generateMLTTokens :: [String] -> [{v:LPToken | lpQty v == 100}] @-}
generateMLTTokens :: [String] -> [LPToken]
generateMLTTokens owners = [ LPToken owner 100 | owner <- owners ]

- **Input:** list of owner names (`[String]`).
- **Output:** list of `LPToken` values, each satisfying the predicate `lpQty v == 100`.  
- The list-comprehension builds exactly those tokens with `100` units, so LH can statically verify the invariant holds.

---

## 4. Why These Refinements Matter

1. **Invariants at the Type Level**  
   Ensures you never accidentally create an `LPToken` with the “wrong” quantity.

2. **Self-Documenting**  
   The type signature itself tells future readers: “All pool tokens are 100 units.”

3. **Compile-time Safety**  
   Liquid Haskell will catch any deviation (e.g. if someone later tried `LPToken owner 50`).

---

### Next Steps

- **Re-enable termination checks** once you add more complex, recursive functions (e.g., SPL token minting logic).
- **Extend refinements** if you introduce other fixed rules (e.g., owner strings matching a pattern or additional token types).

With these annotations, your MLT‐token generator is guaranteed by the type system to always produce tokens of exactly 100 units—giving you strong, compile-time assurances.

```
### 💧 Main.hs
```haskell
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

## 5. 💻 **Getting Started** <a id="getting-started"></a>

### Install GHC & Cabal

```bash
curl -sSf https://get-ghcup.haskell.org | sh
ghcup install ghc 9.2.8
ghcup set ghc 9.2.8
ghcup install cabal
```

### Build & Run

```bash
cabal update
cabal build
cabal run
```

---

## 6. 🧪 **Property-Based Testing** <a id="property-based-testing"></a>

Ensure `.cabal` includes QuickCheck and Tasty:

```bash
cabal test
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

---

## 7. 🧠 **Using GHCi (REPL)** <a id="using-ghci-repl"></a>

```bash
ghci src/Token.hs
:t createToken
```
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
---

## 8. 📜 **Understanding the `.cabal` File** <a id="understanding-the-cabal-file"></a>

Key fields:
- `exposed-modules`: Modules other files can import
- `hs-source-dirs`: Source file directories
- `build-depends`: Required libraries

---

## 9. ⚖️ **GADTs vs LiquidHaskell** <a id="gadts-vs-liquidhaskell"></a>

| Feature              | LiquidHaskell      | GADTs & DataKinds     |
|----------------------|--------------------|-----------------------|
| External tool        | ✅ Yes             | ❌ No (GHC only)      |
| Compile-time safety  | ✅ Yes             | ✅ Yes                |
| Error readability    | ❌ Sometimes cryptic| ✅ GHC messages       |
| Setup complexity     | High               | Low                   |

---

## 10. 📚 **Additional Resources** <a id="additional-resources"></a>

- [GADTs in Haskell](https://wiki.haskell.org/GADT)
- [QuickCheck Docs](https://hackage.haskell.org/package/QuickCheck)
- [Tasty Test Framework](https://hackage.haskell.org/package/tasty)

---

## 11. 🙌 **Final Words** <a id="final-words"></a>

This project helps you **think in types** 🧠—the more logic encoded in types, the fewer runtime bugs you'll encounter. Extend it further with state transitions like trades and swaps. Enjoy building with MSSD! 🍨

## 12. 🙌 Author : Bernard Sibanda
## 12. 🙌 Date :   26-04-2025
## 12. 🙌 Licence : MIT
---------------------------------------------------------------------

