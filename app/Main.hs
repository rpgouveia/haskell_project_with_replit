{-|
Module      : Main
Description : Aplicação principal do sistema de biblioteca
Copyright   : (c) Equipe Haskell, 2024

Interface CLI simples para demonstrar o uso dos módulos da biblioteca.
-}
module Main where

import Library.Types
import Library.Books
import Library.Users
import Library.Loans
import qualified Data.Map.Strict as Map
import Data.Time (fromGregorian, addDays)

main :: IO ()
main = do
    putStrLn "==================================="
    putStrLn "  Sistema de Gerenciamento"
    putStrLn "  de Biblioteca - v0.1.0"
    putStrLn "==================================="
    putStrLn ""
    
    -- Criando dados de exemplo
    let books = Map.fromList
            [ (BookId 1, createBook (BookId 1) "Haskell Programming" "Graham Hutton" "978-1316626221")
            , (BookId 2, createBook (BookId 2) "Learn You a Haskell" "Miran Lipovača" "978-1593272838")
            , (BookId 3, createBook (BookId 3) "Real World Haskell" "Bryan O'Sullivan" "978-0596514983")
            ]
    
    let users = Map.fromList
            [ (UserId 1, createUser (UserId 1) "Alice Silva" "alice@email.com")
            , (UserId 2, createUser (UserId 2) "Bruno Costa" "bruno@email.com")
            , (UserId 3, createUser (UserId 3) "Carla Santos" "carla@email.com")
            ]
    
    let today = fromGregorian 2024 11 1
    let dueDay = addDays 14 today
    
    let loans = Map.fromList
            [ (LoanId 1, createLoan (LoanId 1) (BookId 1) (UserId 1) today dueDay)
            , (LoanId 2, createLoan (LoanId 2) (BookId 3) (UserId 2) today dueDay)
            ]
    
    -- Atualizando disponibilidade dos livros emprestados
    let booksUpdated = markBookAsUnavailable (BookId 1) $
                       markBookAsUnavailable (BookId 3) books
    
    -- Demonstrando funcionalidades
    putStrLn "📚 LIVROS CADASTRADOS:"
    putStrLn "----------------------"
    mapM_ printBook (listAllBooks booksUpdated)
    putStrLn ""
    
    putStrLn $ "✓ Livros disponíveis: " ++ show (countAvailableBooks booksUpdated)
    putStrLn ""
    
    putStrLn "👥 USUÁRIOS CADASTRADOS:"
    putStrLn "------------------------"
    mapM_ printUser (listAllUsers users)
    putStrLn ""
    
    putStrLn $ "✓ Usuários ativos: " ++ show (countActiveUsers users)
    putStrLn ""
    
    putStrLn "📖 EMPRÉSTIMOS ATIVOS:"
    putStrLn "----------------------"
    mapM_ printLoan (listActiveLoans loans)
    putStrLn ""
    
    putStrLn "✓ Sistema funcionando corretamente!"
    putStrLn "✓ Todos os módulos integrados com sucesso!"

printBook :: Book -> IO ()
printBook book = do
    let avail = if available book then "✓ Disponível" else "✗ Emprestado"
    putStrLn $ "  • " ++ title book ++ " - " ++ author book
    putStrLn $ "    ISBN: " ++ isbn book ++ " | " ++ avail

printUser :: User -> IO ()
printUser user = do
    let userStatus = if active user then "✓ Ativo" else "✗ Inativo"
    putStrLn $ "  • " ++ name user ++ " (" ++ email user ++ ")"
    putStrLn $ "    Status: " ++ userStatus

printLoan :: Loan -> IO ()
printLoan loan = do
    putStrLn $ "  • Empréstimo #" ++ show (getLoanId $ loanId loan)
    putStrLn $ "    Livro: " ++ show (getBookId $ loanBookId loan) 
             ++ " | Usuário: " ++ show (getUserId $ loanUserId loan)
    putStrLn $ "    Data: " ++ show (loanDate loan) 
             ++ " | Devolução: " ++ show (dueDate loan)
  where
    getLoanId (LoanId i) = i
    getBookId (BookId i) = i
    getUserId (UserId i) = i
