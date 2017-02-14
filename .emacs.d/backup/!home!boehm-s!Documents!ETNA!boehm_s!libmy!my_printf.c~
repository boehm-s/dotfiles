/*
** my_printf.c for my_printf in /root/Perso/FDI/DEVC/my_printf/boehm_s
** 
** Made by FAHMI Mohamad Kamal
** Login   <fahmi_m@etna-alternance.net>
** 
** Started on  Wed Mar  4 14:37:50 2015 FAHMI Mohamad Kamal
** Last update Sat Nov 12 04:02:49 2016 BOEHM Steven
*/

#include "prototypes.h"

t_fnc	p_fnc[9] =
  {
    {'s', &aff_str},
    {'c', &aff_char},
    {'i', &aff_int},
    {'d', &aff_int},
    {'o', &aff_octal},
    {'u', &aff_int_unsigned},
    {'x', &aff_hexadecimal},
    {'X', &aff_hexadecimal_maj},
    {'\0', NULL}
  };

int	all_print(char arg, va_list *ap)
{
  int		j;
  int		count;

  count = 0;
  j = 0;
  while (p_fnc[j].opt != '\0')
    {
      if (arg == p_fnc[j].opt)
	{
	  count = p_fnc[j].p(ap);
	  return (count);
	}
      j++;
    }
  my_putchar('%');
  my_putchar(arg);
  return (1);
}

int	my_printf(char *str, ...)
{
  va_list	ap;
  int		i;
  int		count;

  if (str == NULL)
    my_putstr_error("<Usage : my_printf(\"%s\", \"str\")>\n");
  count = 1;
  i = 0;
  va_start(ap, str);
  while (str[i] != '\0')
    {
      if (str[i] == '%')
	{
	  i++;
	  (str[i] == '%') ? my_putchar('%') : (count += all_print(str[i], &ap));
	}
      else
	{
	  my_putchar(str[i]);
	  count++;
	}
      i++;
    }
  va_end(ap);
  return (count);
}
