{-|
Module      : Library.BooksSpec
Description : Testes para o módulo Library.Books
-}
module Library.BooksSpec (spec) where

import Test.Hspec
import Library.Types
import Library.Books
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
    describe "Library.Books" $ do
        
        describe "createBook" $ do
            it "cria um livro com todos os campos corretos" $ do
                let book = createBook (BookId 1) "Título" "Autor" "ISBN123"
                title book `shouldBe` "Título"
                author book `shouldBe` "Autor"
                isbn book `shouldBe` "ISBN123"
                available book `shouldBe` True
        
        describe "findBookById" $ do
            it "encontra um livro existente" $ do
                let db = Map.singleton (BookId 1) (createBook (BookId 1) "Test" "Author" "ISBN")
                findBookById (BookId 1) db `shouldSatisfy` maybe False ((== "Test") . title)
            
            it "retorna Nothing para livro inexistente" $ do
                let db = Map.empty
                findBookById (BookId 999) db `shouldBe` Nothing
        
        describe "findBooksByAuthor" $ do
            it "encontra livros pelo autor" $ do
                let book1 = createBook (BookId 1) "Book1" "John Smith" "ISBN1"
                    book2 = createBook (BookId 2) "Book2" "Jane Doe" "ISBN2"
                    book3 = createBook (BookId 3) "Book3" "John Doe" "ISBN3"
                    db = Map.fromList [(BookId 1, book1), (BookId 2, book2), (BookId 3, book3)]
                length (findBooksByAuthor "John" db) `shouldBe` 2
            
            it "retorna lista vazia quando não encontra o autor" $ do
                let db = Map.singleton (BookId 1) (createBook (BookId 1) "Test" "Author" "ISBN")
                findBooksByAuthor "Nonexistent" db `shouldBe` []
        
        describe "markBookAsAvailable/Unavailable" $ do
            it "marca livro como indisponível" $ do
                let book = createBook (BookId 1) "Test" "Author" "ISBN"
                    db = Map.singleton (BookId 1) book
                    updated = markBookAsUnavailable (BookId 1) db
                case findBookById (BookId 1) updated of
                    Just b -> available b `shouldBe` False
                    Nothing -> expectationFailure "Livro não encontrado"
            
            it "marca livro como disponível" $ do
                let book = (createBook (BookId 1) "Test" "Author" "ISBN") { available = False }
                    db = Map.singleton (BookId 1) book
                    updated = markBookAsAvailable (BookId 1) db
                case findBookById (BookId 1) updated of
                    Just b -> available b `shouldBe` True
                    Nothing -> expectationFailure "Livro não encontrado"
        
        describe "countAvailableBooks" $ do
            it "conta corretamente livros disponíveis" $ do
                let book1 = createBook (BookId 1) "Book1" "Author1" "ISBN1"
                    book2 = (createBook (BookId 2) "Book2" "Author2" "ISBN2") { available = False }
                    book3 = createBook (BookId 3) "Book3" "Author3" "ISBN3"
                    db = Map.fromList [(BookId 1, book1), (BookId 2, book2), (BookId 3, book3)]
                countAvailableBooks db `shouldBe` 2
