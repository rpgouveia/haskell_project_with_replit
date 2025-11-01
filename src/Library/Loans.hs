{-|
Module      : Library.Loans
Description : Operações relacionadas a empréstimos

Este módulo fornece funcionalidades para gerenciar empréstimos de livros.
-}
module Library.Loans
    ( createLoan
    , returnLoan
    , findLoanById
    , findLoansByUser
    , findLoansByBook
    , listActiveLoans
    , listOverdueLoans
    , isOverdue
    , daysOverdue
    ) where

import Library.Types
import Data.Time (Day, diffDays)
import qualified Data.Map.Strict as Map

type LoanDatabase = Map.Map LoanId Loan

-- | Cria um novo empréstimo
createLoan :: LoanId -> BookId -> UserId -> Day -> Day -> Loan
createLoan lid bid uid start due = Loan
    { loanId = lid
    , loanBookId = bid
    , loanUserId = uid
    , loanDate = start
    , dueDate = due
    , returnDate = Nothing
    , status = Active
    }

-- | Registra a devolução de um livro
returnLoan :: LoanId -> Day -> LoanDatabase -> LoanDatabase
returnLoan lid returnDay = Map.adjust updateLoan lid
  where
    updateLoan loan = loan 
        { returnDate = Just returnDay
        , status = Returned
        }

-- | Busca um empréstimo pelo ID
findLoanById :: LoanId -> LoanDatabase -> Maybe Loan
findLoanById = Map.lookup

-- | Busca todos os empréstimos de um usuário
findLoansByUser :: UserId -> LoanDatabase -> [Loan]
findLoansByUser uid db =
    filter (\l -> loanUserId l == uid) (Map.elems db)

-- | Busca todos os empréstimos de um livro
findLoansByBook :: BookId -> LoanDatabase -> [Loan]
findLoansByBook bid db =
    filter (\l -> loanBookId l == bid) (Map.elems db)

-- | Lista todos os empréstimos ativos
listActiveLoans :: LoanDatabase -> [Loan]
listActiveLoans = filter (\l -> status l == Active) . Map.elems

-- | Lista empréstimos atrasados
listOverdueLoans :: Day -> LoanDatabase -> [Loan]
listOverdueLoans today db =
    filter (isOverdue today) (listActiveLoans db)

-- | Verifica se um empréstimo está atrasado
isOverdue :: Day -> Loan -> Bool
isOverdue today loan =
    status loan == Active && today > dueDate loan

-- | Calcula dias de atraso
daysOverdue :: Day -> Loan -> Integer
daysOverdue today loan
    | isOverdue today loan = diffDays today (dueDate loan)
    | otherwise = 0