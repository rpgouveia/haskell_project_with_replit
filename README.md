# Haskell Library Manager

Sistema simples de gerenciamento de biblioteca desenvolvido em Haskell, demonstrando desenvolvimento colaborativo com múltiplos módulos.

## 📋 Estrutura do Projeto

O projeto está organizado em 4 módulos principais, cada um desenvolvido por um membro diferente da equipe:

- **Library.Types** (Desenvolvedor 1): Tipos básicos e estruturas de dados
- **Library.Books** (Desenvolvedor 2): Operações relacionadas a livros
- **Library.Users** (Desenvolvedor 3): Gerenciamento de usuários
- **Library.Loans** (Desenvolvedor 4): Sistema de empréstimos

## 🛠️ Requisitos

- GHC 9.12.2
- Cabal 3.16.0.0
- GHCup 0.1.50.2

## 🚀 Como Executar

### Compilar o projeto

```bash
cabal build
```

### Executar a aplicação

```bash
cabal run haskell-library-manager
```

### Executar os testes

```bash
cabal test
```

### Executar os testes com detalhes

```bash
cabal test --test-show-details=direct
```

## 📦 Estrutura de Diretórios

```
.
├── app/
│   └── Main.hs                    # Aplicação principal
├── src/
│   └── Library/
│       ├── Types.hs               # Tipos básicos
│       ├── Books.hs               # Operações com livros
│       ├── Users.hs               # Operações com usuários
│       └── Loans.hs               # Operações com empréstimos
├── test/
│   ├── Spec.hs                    # Descoberta automática de testes
│   └── Library/
│       ├── BooksSpec.hs           # Testes de livros
│       ├── UsersSpec.hs           # Testes de usuários
│       └── LoansSpec.hs           # Testes de empréstimos
├── haskell-library-manager.cabal  # Configuração do projeto
└── README.md                      # Este arquivo
```

## 🧪 Testes

O projeto utiliza HSpec para testes. Cada módulo possui seu próprio conjunto de testes:

- **BooksSpec**: Testa operações com livros (criação, busca, disponibilidade)
- **UsersSpec**: Testa gerenciamento de usuários (ativação, busca por email)
- **LoansSpec**: Testa sistema de empréstimos (criação, devolução, atrasos)

## 📚 Funcionalidades

### Livros
- Cadastro de livros
- Busca por ID, autor ou título
- Controle de disponibilidade
- Contagem de livros disponíveis

### Usuários
- Cadastro de usuários
- Busca por ID ou email
- Ativação/desativação de usuários
- Listagem de usuários ativos

### Empréstimos
- Criação de empréstimos
- Registro de devolução
- Busca por usuário ou livro
- Detecção de empréstimos atrasados

## 🔧 Desenvolvimento

Para adicionar novas funcionalidades:

1. Adicione os tipos necessários em `Library.Types`
2. Implemente as funções no módulo apropriado
3. Adicione testes no arquivo `*Spec.hs` correspondente
4. Atualize o `Main.hs` se necessário

## 📄 Licença

MIT

## 👥 Equipe

- Desenvolvedor 1: Sistema de tipos
- Desenvolvedor 2: Módulo de livros
- Desenvolvedor 3: Módulo de usuários
- Desenvolvedor 4: Módulo de empréstimos
