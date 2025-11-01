.PHONY: build run test clean install

# Compila o projeto
build:
	cabal build

# Executa a aplicação
run:
	cabal run haskell-library-manager

# Executa os testes
test:
	cabal test

# Executa os testes com detalhes
test-verbose:
	cabal test --test-show-details=direct

# Limpa os arquivos compilados
clean:
	cabal clean

# Instala dependências
install:
	cabal update
	cabal build --only-dependencies

# Configura o projeto
setup:
	cabal update
	cabal configure --enable-tests

# Executa tudo (build + test + run)
all: build test run

help:
	@echo "Comandos disponíveis:"
	@echo "  make build         - Compila o projeto"
	@echo "  make run          - Executa a aplicação"
	@echo "  make test         - Executa os testes"
	@echo "  make test-verbose - Executa os testes com detalhes"
	@echo "  make clean        - Remove arquivos compilados"
	@echo "  make install      - Instala dependências"
	@echo "  make setup        - Configura o projeto"
	@echo "  make all          - Build + Test + Run"
