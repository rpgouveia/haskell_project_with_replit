{-|
Module      : Library.Types
Description : Tipos básicos do sistema de biblioteca
Copyright   : (c) Desenvolvedor 1, 2024
Maintainer  : dev1@example.com

Este módulo define os tipos fundamentais usados em todo o sistema.
-}
module Library.Types
    ( BookId(..)
    , UserId(..)
    , LoanId(..)
    , Book(..)
    , User(..)
    , Loan(..)
    , LoanStatus(..)
    ) where

import Data.Time (Day)

-- | Identificador único de livro
newtype BookId = BookId Int
    deriving (Eq, Ord, Show)

-- | Identificador único de usuário
newtype UserId = UserId Int
    deriving (Eq, Ord, Show)

-- | Identificador único de empréstimo
newtype LoanId = LoanId Int
    deriving (Eq, Ord, Show)

-- | Representação de um livro
data Book = Book
    { bookId     :: BookId
    , title      :: String
    , author     :: String
    , isbn       :: String
    , available  :: Bool
    } deriving (Eq, Show)

-- | Representação de um usuário
data User = User
    { userId   :: UserId
    , name     :: String
    , email    :: String
    , active   :: Bool
    } deriving (Eq, Show)

-- | Status de um empréstimo
data LoanStatus = Active | Returned | Overdue
    deriving (Eq, Show)

-- | Representação de um empréstimo
data Loan = Loan
    { loanId     :: LoanId
    , loanBookId :: BookId
    , loanUserId :: UserId
    , loanDate   :: Day
    , dueDate    :: Day
    , returnDate :: Maybe Day
    , status     :: LoanStatus
    } deriving (Eq, Show)
