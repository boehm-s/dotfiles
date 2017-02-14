##
## Makefile for  in /home/boehm_s/DEV_C/Libmy/boehm_s/libmy_01
## 
## Made by BOEHM Steven
## Login   <boehm_s@etna-alternance.net>
## 
## Started on  Sat Oct  4 18:59:41 2014 BOEHM Steven
## Last update Wed Oct  8 09:40:27 2014 BOEHM Steven
##

AR	= ar r
LIB	= ranlib
NAME	= libmy.a
SRC	= 	my_putchar.c	\
		my_isneg.c	\
		my_put_nbr.c	\
		my_swap.c	\
		my_putstr.c	\
		my_strlen.c	\
		my_getnbr.c	\
		my_aff_tab.c	\
		my_strcpy.c	\
		my_strncpy.c	\
		my_strcmp.c	\
		my_strncmp.c	\
		my_strcat.c	\
		my_strncat.c	\
		my_strstr.c	\
		my_putnbr_base.c\
		my_strdup.c	\
		my_str_to_wordtab.c \
		aff_functions.c \
		aff_functions2.c \
		classics.c	\
		classics2.c	\
		my_printf.c	\
		my_pow.c	\

OBJ	= $(SRC:%.c=%.o)

$(NAME):	$(OBJ)
		$(AR) $(NAME) $(OBJ)
		ranlib $(NAME)
all:		$(NAME)
clean:
		$(RM) $(OBJ)
fclean:		clean
		$(RM) $(NAME)
re: 		fclean all
