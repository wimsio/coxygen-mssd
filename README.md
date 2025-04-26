
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