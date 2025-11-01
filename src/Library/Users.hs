{-|
Module      : Library.Users
Description : Operações relacionadas a usuários
Copyright   : (c) Desenvolvedor 3, 2024
Maintainer  : dev3@example.com

Este módulo fornece funcionalidades para gerenciar usuários da biblioteca.
-}
module Library.Users
    ( createUser
    , findUserById
    , findUserByEmail
    , activateUser
    , deactivateUser
    , listAllUsers
    , listActiveUsers
    , countActiveUsers
    ) where

import Library.Types
import qualified Data.Map.Strict as Map

type UserDatabase = Map.Map UserId User

-- | Cria um novo usuário
createUser :: UserId -> String -> String -> User
createUser uid n e = User
    { userId = uid
    , name = n
    , email = e
    , active = True
    }

-- | Busca um usuário pelo ID
findUserById :: UserId -> UserDatabase -> Maybe User
findUserById = Map.lookup

-- | Busca um usuário pelo email
findUserByEmail :: String -> UserDatabase -> Maybe User
findUserByEmail emailQuery db =
    case filter (\u -> email u == emailQuery) (Map.elems db) of
        []    -> Nothing
        (u:_) -> Just u

-- | Ativa um usuário
activateUser :: UserId -> UserDatabase -> UserDatabase
activateUser uid = Map.adjust (\u -> u { active = True }) uid

-- | Desativa um usuário
deactivateUser :: UserId -> UserDatabase -> UserDatabase
deactivateUser uid = Map.adjust (\u -> u { active = False }) uid

-- | Lista todos os usuários
listAllUsers :: UserDatabase -> [User]
listAllUsers = Map.elems

-- | Lista apenas usuários ativos
listActiveUsers :: UserDatabase -> [User]
listActiveUsers = filter active . Map.elems

-- | Conta quantos usuários estão ativos
countActiveUsers :: UserDatabase -> Int
countActiveUsers = length . listActiveUsers
