
# 🍨 MSSD — Mockup Sundae Swap DEX (Type-Safe Haskell Project)

Welcome to the **MSSD** project — a beginner-friendly, strongly typed mock decentralized exchange built in **pure Haskell**. This project uses GADTs, smart constructors, and property-based testing to teach you core functional programming concepts and formal correctness.

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
- git commit -m "Updated the Readme file by adding git steps for new developers"
- git push origin main

```

---

## 🧱 Modules Overview

### 🎫 `Token.hs`
Defines a token with a name and quantity. Enforced invariant: `quantity > 0`.

```haskell
data Token (q :: QuantityStatus) where
  SafeToken :: { tokenName :: String, quantity :: Integer } -> Token 'Valid

createToken :: String -> Integer -> Maybe (Token 'Valid)
```

---

### 👤 `User.hs`
Defines a user with a unique ID, name, and list of tokens. Enforced: `userId >= 0`.

```haskell
data User (s :: UserStatus) where
  SafeUser :: { userId :: Int, username :: String, holdings :: [Token 'Valid] } -> User 'Good

createUser :: Int -> String -> Maybe (User 'Good)
```

---

### 💱 `Exchange.hs`
Defines a listing for a token with a price. Enforced: `price > 0`.

```haskell
data Listing (p :: PriceStatus) where
  SafeListing :: Token 'Valid -> Double -> Listing 'Positive

createListing :: Token 'Valid -> Double -> Maybe (Listing 'Positive)
```

---

### 💧 `LP.hs`
Liquidity provider tokens with fixed quantity of 100.

```haskell
data LPToken = LPToken { lpOwner :: String, lpQty :: Integer }

generateMLTTokens :: [String] -> [LPToken]
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
import Test.Tasty
import Test.Tasty.QuickCheck as QC

main = defaultMain tests

tests :: TestTree
tests = testGroup "QuickCheck Tests"
  [ QC.testProperty "Token quantity must be > 0" $
      \qty -> qty > 0 QC.==> tokenQuantity (SafeToken "T" qty) > 0
  ]
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
cabal-version: 3.0
name: MSSD
version: 0.1.0.0

library
  hs-source-dirs: src
  exposed-modules: Token, User, Exchange, LP
  build-depends: base >=4.15 && <5
  default-language: Haskell2010

executable MSSD-exe
  main-is: Main.hs
  hs-source-dirs: app
  build-depends: base, MSSD
  default-language: Haskell2010
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