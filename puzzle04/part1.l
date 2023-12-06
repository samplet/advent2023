/* part1.l: Scanner for Advent of Code 2023 4.1
 * Copyright 2023 Timothy Sample <samplet@ngyro.com>
 * SPDX-License-Identifier: CC0-1.0
 */

%{
  #include "part1.tab.h"
%}

%%

[ \t\n] {}
Card { return CARD; }
[[:digit:]]+ { yylval = atoi (yytext); return NUMBER; }
":" { return COLON; }
"|" { return PIPE; }

%%
