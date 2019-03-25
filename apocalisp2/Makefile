##
## Makefile for getStartedWithHaskell in /Users/hadibereksi/Documents/Epitech/TEK2/FunctionalProgramming/getStartedWithHaskell
##
## Made by test
## Login   <>
##
## Started on  Mon Mar 25 23:25:54 2019 test
## Last update Mon Mar 25 23:25:54 2019 test
##

NAME	=	lisp2

SRC	=	app/Main.hs		\

all:	$(NAME)

$(NAME): $(SRC)
	stack build --copy-bins --local-bin-path .

clean:
	stack clean

fclean:	clean
	$(RM) $(NAME)

re:	fclean all

.PHONY: all clean fclean re
