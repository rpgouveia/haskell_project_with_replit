{-|
Module      : Library.UsersSpec
Description : Testes para o módulo Library.Users
-}
module Library.UsersSpec (spec) where

import Test.Hspec
import Library.Types
import Library.Users
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
    describe "Library.Users" $ do
        
        describe "createUser" $ do
            it "cria um usuário com todos os campos corretos" $ do
                let user = createUser (UserId 1) "João Silva" "joao@email.com"
                name user `shouldBe` "João Silva"
                email user `shouldBe` "joao@email.com"
                active user `shouldBe` True
        
        describe "findUserById" $ do
            it "encontra um usuário existente" $ do
                let db = Map.singleton (UserId 1) (createUser (UserId 1) "Test" "test@email.com")
                findUserById (UserId 1) db `shouldSatisfy` maybe False ((== "Test") . name)
            
            it "retorna Nothing para usuário inexistente" $ do
                let db = Map.empty
                findUserById (UserId 999) db `shouldBe` Nothing
        
        describe "findUserByEmail" $ do
            it "encontra usuário pelo email" $ do
                let user = createUser (UserId 1) "Test User" "test@email.com"
                    db = Map.singleton (UserId 1) user
                findUserByEmail "test@email.com" db `shouldSatisfy` maybe False ((== "Test User") . name)
            
            it "retorna Nothing quando email não existe" $ do
                let db = Map.singleton (UserId 1) (createUser (UserId 1) "Test" "test@email.com")
                findUserByEmail "notfound@email.com" db `shouldBe` Nothing
        
        describe "activateUser/deactivateUser" $ do
            it "desativa um usuário" $ do
                let user = createUser (UserId 1) "Test" "test@email.com"
                    db = Map.singleton (UserId 1) user
                    updated = deactivateUser (UserId 1) db
                case findUserById (UserId 1) updated of
                    Just u -> active u `shouldBe` False
                    Nothing -> expectationFailure "Usuário não encontrado"
            
            it "ativa um usuário" $ do
                let user = (createUser (UserId 1) "Test" "test@email.com") { active = False }
                    db = Map.singleton (UserId 1) user
                    updated = activateUser (UserId 1) db
                case findUserById (UserId 1) updated of
                    Just u -> active u `shouldBe` True
                    Nothing -> expectationFailure "Usuário não encontrado"
        
        describe "listActiveUsers" $ do
            it "lista apenas usuários ativos" $ do
                let user1 = createUser (UserId 1) "User1" "user1@email.com"
                    user2 = (createUser (UserId 2) "User2" "user2@email.com") { active = False }
                    user3 = createUser (UserId 3) "User3" "user3@email.com"
                    db = Map.fromList [(UserId 1, user1), (UserId 2, user2), (UserId 3, user3)]
                length (listActiveUsers db) `shouldBe` 2
        
        describe "countActiveUsers" $ do
            it "conta corretamente usuários ativos" $ do
                let user1 = createUser (UserId 1) "User1" "user1@email.com"
                    user2 = (createUser (UserId 2) "User2" "user2@email.com") { active = False }
                    user3 = createUser (UserId 3) "User3" "user3@email.com"
                    db = Map.fromList [(UserId 1, user1), (UserId 2, user2), (UserId 3, user3)]
                countActiveUsers db `shouldBe` 2
