EXE_NAME := ft_lex

LIB_NAME := libl


all: $(EXE_NAME) $(LIB_NAME)


$(EXE_NAME):
	cabal build
	ln -s ./dist-newstyle/build/x86_64-linux/ghc-9.6.7/ft-lex-0.1.0.0/x/ft-lex/build/ft-lex/ft-lex  ft_lex 		

$(LIB_NAME):
	gcc -shared -fPIC lib/libl.c -o lib/libl.so

compile: ## This is used to compile the .c with the .so
	gcc test.c -L ./lib -ll -Wl,-rpath=./lib

.PHONY: compile all
