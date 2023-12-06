/* part1.y: Solve Advent of Code 2023 4.1
 * Copyright 2023 Timothy Sample <samplet@ngyro.com>
 * SPDX-License-Identifier: CC0-1.0
 */

%{
  #include <stdio.h>
  #include <stdlib.h>

  #define WINNER_LIMIT 128

  int card_id;
  int winners[WINNER_LIMIT];
  int sum;
  int count;

  void add_winner (int n);
  int is_winner (int n);
  void clear_winners ();
%}

%token CARD NUMBER COLON PIPE

%%

cards: %empty | cards card;

card: header winners PIPE numbers {
  if (count > 0)
    sum += 1 << (count - 1);
};

header: CARD NUMBER COLON {
  card_id = $2;
  clear_winners ();
  count = 0;
};

winners: %empty | winners NUMBER {
  add_winner ($2);
};

numbers: %empty | numbers NUMBER {
  if (is_winner ($2))
    count++;
};

%%

void
add_winner (int n)
{
  for (int i = 0; i < WINNER_LIMIT - 1; i++)
    if (winners[i] == -1)
      {
        winners[i] = n;
        winners[i + 1] = -1;
        return;
      }
  exit (EXIT_FAILURE);
}

int
is_winner (int n)
{
  for (int i = 0; i < WINNER_LIMIT; i++)
    if (winners[i] == n)
      return 1;
    else if (winners[i] == -1)
      return 0;
  exit (EXIT_FAILURE);
}

void
clear_winners ()
{
  winners[0] = -1;
}

void
yyerror (char *s)
{
  fprintf (stderr, "%s\n", s);
}

int
main ()
{
  sum = 0;
  count = 0;
  clear_winners ();
  yyparse ();
  printf ("%d\n", sum);
}
