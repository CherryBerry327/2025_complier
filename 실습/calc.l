%{
/* LEX source for calculator program */
#include "calc.h"
%}
%%
[ \t]	; /* ignore blanks and tabs */
[0-9]+	{yylval = atoi(yytext); return NUMBER;}
"mod"	return MOD;
"div"	return DIV;
"sqr"	return SQR;
\n|. 	return yytext[0];  /* return everything else */
%%
int yywrap() {return(0);}
