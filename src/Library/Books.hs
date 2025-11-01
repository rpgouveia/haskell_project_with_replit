{-|
Module      : Library.Books
Description : Operações relacionadas a livros
Copyright   : (c) Desenvolvedor 2, 2024
Maintainer  : dev2@example.com

Este módulo fornece funcionalidades para gerenciar livros na biblioteca.
-}
module Library.Books
    ( createBook
    , findBookById
    , findBooksByAuthor
    , findBooksByTitle
    , markBookAsAvailable
    , markBookAsUnavailable
    , listAllBooks
    , countAvailableBooks
    ) where

import Library.Types
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map

type BookDatabase = Map.Map BookId Book

-- | Cria um novo livro
createBook :: BookId -> String -> String -> String -> Book
createBook bid t a i = Book
    { bookId = bid
    , title = t
    , author = a
    , isbn = i
    , available = True
    }

-- | Busca um livro pelo ID
findBookById :: BookId -> BookDatabase -> Maybe Book
findBookById = Map.lookup

-- | Busca livros por autor (busca parcial)
findBooksByAuthor :: String -> BookDatabase -> [Book]
findBooksByAuthor authorName db =
    filter (\b -> authorName `isInfixOf` author b) (Map.elems db)

-- | Busca livros por título (busca parcial)
findBooksByTitle :: String -> BookDatabase -> [Book]
findBooksByTitle titleQuery db =
    filter (\b -> titleQuery `isInfixOf` title b) (Map.elems db)

-- | Marca um livro como disponível
markBookAsAvailable :: BookId -> BookDatabase -> BookDatabase
markBookAsAvailable bid = Map.adjust (\b -> b { available = True }) bid

-- | Marca um livro como indisponível
markBookAsUnavailable :: BookId -> BookDatabase -> BookDatabase
markBookAsUnavailable bid = Map.adjust (\b -> b { available = False }) bid

-- | Lista todos os livros
listAllBooks :: BookDatabase -> [Book]
listAllBooks = Map.elems

-- | Conta quantos livros estão disponíveis
countAvailableBooks :: BookDatabase -> Int
countAvailableBooks = length . filter available . Map.elems
