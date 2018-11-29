-- start snippet imports
module Examples.Traversals.Writing where

import           Control.Lens
import qualified Data.Map                      as M
-- end snippet imports


-- start snippet transaction
data Transaction =
    Withdrawal {amount :: Int}
    | Deposit {amount :: Int }
  deriving Show
-- end snippet transaction

-- start snippet simple-traversal
simpleTransactions :: Traversal' [Transaction] Transaction
simpleTransactions = traverse
-- end snippet simple-traversal

someTransactions :: [Transaction]
someTransactions = [Deposit 100, Withdrawal 50]

-- start snippet type-changing-traversal
typeChangingTransactions
  :: Traversal [Transaction] [result] Transaction result
typeChangingTransactions = traverse
-- end snippet type-changing-traversal

-- start snippet all-amounts
allAmounts
  :: (Applicative f)
  => (Int -> f Int)
  -> [Transaction]
  -> f [Transaction]
allAmounts f ts = traverse go ts
 where
  go (Withdrawal amt) = Withdrawal <$> f amt
  go (Deposit    amt) = Deposit <$> f amt
-- end snippet all-amounts


-- start snippet bad-withdrawals
badWithdrawals :: Traversal' [Transaction] Int
badWithdrawals f ts = traverse go (filter isWithdrawal ts)
 where
  isWithdrawal Withdrawal{} = True
  isWithdrawal _            = False
  go t = Withdrawal <$> f (amount t)
-- end snippet bad-withdrawals

-- start snippet withdrawals
-- withdrawals :: Traversal' [Transaction] Int
-- a.k.a.
withdrawals
  :: (Applicative f)
  => (Int -> f Int)
  -> [Transaction]
  -> f [Transaction]
withdrawals f ts = traverse go ts
 where
  go (Withdrawal amt) = Withdrawal <$> f amt
  go (Deposit    amt) = Deposit <$> pure amt
-- end snippet withdrawals


-- start snippet bank-account
data AccountType = Chequing | Savings
  deriving Show

data BankAccount =
  BankAccount
    { accountType :: AccountType
    , transactions :: [Transaction]
    } deriving Show
-- end snippet bank-account


-- start snippet account-withdrawals
-- accountWithdrawals :: Traversal' BankAccount Int
-- a.k.a.
accountWithdrawals
  :: (Applicative f) => (Int -> f Int) -> BankAccount -> f BankAccount
accountWithdrawals f (BankAccount type' transactions') =
  BankAccount type' <$> withdrawals f transactions'
-- end snippet account-withdrawals

type AccountNumber = Int

accountTransactions :: Traversal' BankAccount Transaction
accountTransactions f (BankAccount type' ts) =
  BankAccount type' <$> traverse f ts

account :: BankAccount
account = BankAccount Savings [Deposit 100, Withdrawal 50]

data BankProfile =
  BankProfile
    { _primaryAccountHolder :: String
    , _secondaryAccountHolder :: Maybe String
    , _accounts :: M.Map AccountNumber BankAccount
    } deriving Show

-- accountHolders :: Traversal' BankProfile String
-- accountHolders f bp =
--   BankProfile
--     <$> f (bp ^. primaryAccountHolder)
--     <*> traverse f (bp ^. secondaryAccountHolder)
--     <*> pure (bp ^. accounts)

profile :: BankProfile
profile = BankProfile
  { _primaryAccountHolder   = "Joe"
  , _secondaryAccountHolder = Just "Stacy"
  , _accounts               = M.empty
  }
