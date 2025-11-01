{-|
Module      : Library.LoansSpec
Description : Testes para o módulo Library.Loans
-}
module Library.LoansSpec (spec) where

import Test.Hspec
import Library.Types
import Library.Loans
import qualified Data.Map.Strict as Map
import Data.Time (fromGregorian, addDays)

spec :: Spec
spec = do
    describe "Library.Loans" $ do
        let today = fromGregorian 2024 11 1
        let dueDay = addDays 14 today
        let pastDue = addDays (-1) today
        
        describe "createLoan" $ do
            it "cria um empréstimo com status Active" $ do
                let loan = createLoan (LoanId 1) (BookId 1) (UserId 1) today dueDay
                status loan `shouldBe` Active
                returnDate loan `shouldBe` Nothing
        
        describe "returnLoan" $ do
            it "registra a devolução de um livro" $ do
                let loan = createLoan (LoanId 1) (BookId 1) (UserId 1) today dueDay
                    db = Map.singleton (LoanId 1) loan
                    returnDay = addDays 7 today
                    updated = returnLoan (LoanId 1) returnDay db
                case findLoanById (LoanId 1) updated of
                    Just l -> do
                        status l `shouldBe` Returned
                        returnDate l `shouldBe` Just returnDay
                    Nothing -> expectationFailure "Empréstimo não encontrado"
        
        describe "findLoansByUser" $ do
            it "encontra todos os empréstimos de um usuário" $ do
                let loan1 = createLoan (LoanId 1) (BookId 1) (UserId 1) today dueDay
                    loan2 = createLoan (LoanId 2) (BookId 2) (UserId 2) today dueDay
                    loan3 = createLoan (LoanId 3) (BookId 3) (UserId 1) today dueDay
                    db = Map.fromList [(LoanId 1, loan1), (LoanId 2, loan2), (LoanId 3, loan3)]
                length (findLoansByUser (UserId 1) db) `shouldBe` 2
        
        describe "findLoansByBook" $ do
            it "encontra todos os empréstimos de um livro" $ do
                let loan1 = createLoan (LoanId 1) (BookId 1) (UserId 1) today dueDay
                    loan2 = createLoan (LoanId 2) (BookId 2) (UserId 2) today dueDay
                    loan3 = createLoan (LoanId 3) (BookId 1) (UserId 3) today dueDay
                    db = Map.fromList [(LoanId 1, loan1), (LoanId 2, loan2), (LoanId 3, loan3)]
                length (findLoansByBook (BookId 1) db) `shouldBe` 2
        
        describe "listActiveLoans" $ do
            it "lista apenas empréstimos ativos" $ do
                let loan1 = createLoan (LoanId 1) (BookId 1) (UserId 1) today dueDay
                    loan2 = createLoan (LoanId 2) (BookId 2) (UserId 2) today dueDay
                    db = Map.fromList [(LoanId 1, loan1), (LoanId 2, loan2)]
                    returnDay = addDays 7 today
                    updated = returnLoan (LoanId 1) returnDay db
                length (listActiveLoans updated) `shouldBe` 1
        
        describe "isOverdue" $ do
            it "detecta empréstimo atrasado" $ do
                let loan = createLoan (LoanId 1) (BookId 1) (UserId 1) today pastDue
                    checkDay = addDays 1 today
                isOverdue checkDay loan `shouldBe` True
            
            it "não marca empréstimo dentro do prazo como atrasado" $ do
                let loan = createLoan (LoanId 1) (BookId 1) (UserId 1) today dueDay
                isOverdue today loan `shouldBe` False
            
            it "não marca empréstimo devolvido como atrasado" $ do
                let loan = createLoan (LoanId 1) (BookId 1) (UserId 1) today pastDue
                    db = Map.singleton (LoanId 1) loan
                    returnDay = today
                    updated = returnLoan (LoanId 1) returnDay db
                case findLoanById (LoanId 1) updated of
                    Just l -> isOverdue (addDays 10 today) l `shouldBe` False
                    Nothing -> expectationFailure "Empréstimo não encontrado"
        
        describe "listOverdueLoans" $ do
            it "lista empréstimos atrasados" $ do
                let loan1 = createLoan (LoanId 1) (BookId 1) (UserId 1) today pastDue
                    loan2 = createLoan (LoanId 2) (BookId 2) (UserId 2) today dueDay
                    db = Map.fromList [(LoanId 1, loan1), (LoanId 2, loan2)]
                    checkDay = addDays 1 today
                length (listOverdueLoans checkDay db) `shouldBe` 1
