# Setup para Replit

Este guia mostra como configurar e executar o projeto no Replit.

## 🚀 Quick Start no Replit

### 1. Importar o Projeto

1. Vá para [Replit](https://replit.com)
2. Clique em "Create Repl"
3. Selecione "Import from GitHub"
4. Cole a URL do seu repositório: `https://github.com/rpgouveia/haskell_project_with_replit`
5. Clique em "Import from GitHub"

### 2. Configuração Automática

O Replit detectará automaticamente o arquivo `.replit` e `replit.nix` e configurará o ambiente com:
- GHC (Glasgow Haskell Compiler)
- Cabal (build tool)
- Haskell Language Server
- HSpec Discover

### 3. Primeiro Build

No terminal do Replit, execute:

```bash
cabal update
cabal build
```

Isso pode levar alguns minutos na primeira vez, pois vai baixar e compilar todas as dependências.

### 4. Executar o Projeto

Você pode executar o projeto de várias formas:

**Opção 1: Usando o botão Run do Replit**
- Simplesmente clique no botão "Run" ▶️ no topo da tela

**Opção 2: Usando comandos do Cabal**
```bash
cabal run haskell-library-manager
```

**Opção 3: Usando o Makefile**
```bash
make run
```

### 5. Executar os Testes

```bash
# Testes simples
cabal test

# Testes com saída detalhada
cabal test --test-show-details=direct

# Ou usando o Makefile
make test
make test-verbose
```

## 📝 Comandos Úteis

### Usando Cabal
```bash
# Atualizar pacotes
cabal update

# Compilar o projeto
cabal build

# Executar a aplicação
cabal run haskell-library-manager

# Executar testes
cabal test

# Limpar build
cabal clean

# Instalar dependências apenas
cabal build --only-dependencies
```

### Usando Make (mais conveniente)
```bash
make help          # Ver todos os comandos disponíveis
make build         # Compilar
make run           # Executar
make test          # Testar
make test-verbose  # Testar com detalhes
make clean         # Limpar
make all           # Build + Test + Run
```

## 🔧 Troubleshooting

### Problema: "cabal: command not found"

O Replit deve configurar automaticamente o ambiente. Se isso não acontecer:

1. Verifique se os arquivos `.replit` e `replit.nix` estão presentes
2. Tente recarregar o Repl
3. Se necessário, adicione manualmente no shell:

```bash
nix-env -iA nixpkgs.cabal-install
nix-env -iA nixpkgs.ghc
```

### Problema: Dependências não encontradas

Execute:
```bash
cabal update
cabal build --only-dependencies
```

### Problema: Testes falhando

Certifique-se de que o hspec-discover está instalado:
```bash
cabal install hspec-discover
```

## 🎯 Estrutura do Projeto no Replit

```
haskell_project_with_replit/
├── .replit                    # Configuração do Replit
├── replit.nix                 # Dependências Nix
├── Makefile                   # Comandos convenientes
├── haskell-library-manager.cabal
├── cabal.project
├── README.md
├── SETUP.md                   # Este arquivo
├── app/
│   └── Main.hs
├── src/
│   └── Library/
│       ├── Types.hs
│       ├── Books.hs
│       ├── Users.hs
│       └── Loans.hs
└── test/
    ├── Spec.hs
    └── Library/
        ├── BooksSpec.hs
        ├── UsersSpec.hs
        └── LoansSpec.hs
```

## 💡 Dicas para Desenvolvimento no Replit

1. **Editor**: O Replit tem syntax highlighting para Haskell
2. **Terminal**: Use o terminal integrado para comandos
3. **Hot Reload**: Após modificar código, basta executar `cabal run` novamente
4. **Depuração**: Use `putStrLn` e `print` para debug
5. **Compartilhamento**: Você pode compartilhar seu Repl com outros desenvolvedores

## 🧪 Testando Individualmente

Para testar módulos específicos no REPL:

```bash
cabal repl

# No REPL:
:load Library.Books
:load Library.Users
:load Library.Loans
```

## 📚 Recursos Adicionais

- [Documentação do Cabal](https://cabal.readthedocs.io/)
- [Documentação do Haskell](https://www.haskell.org/documentation/)
- [HSpec Documentation](https://hspec.github.io/)
- [Replit Haskell Template](https://replit.com/languages/haskell)
