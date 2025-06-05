%{
#define YY_NO_UNISTD_H 1 
#include <string.h>
#include "cbc3.h"
#define MAXSYM	100
#define	MAXSYMLEN	20
char symtbl[MAXSYM][MAXSYMLEN];
int lineno=1;
int insertsym();
int maxsym=0;
char s[MAXSYMLEN];
int temp;

%}
sp		[ \t]
ws		{sp}+
nl		\n
eletter     [A-Za-z_]
hletter		[\xb0-\xfe][\xa0-\xfe]
letter		({eletter}|{hletter})
digit		[0-9]
id		{letter}({letter}|{digit}|_)*
%%
{ws}		{/* do nothing */}
{nl}		{lineno++; }
\+		{return(ADD); }
\-		{return(SUB); }
\*		{return(MUL); }     // 곱하기(*) 연산자 규칙 추가, MUL 토큰 반환
\/		{return(DIV); }     // 나누기(/) 연산자 규칙 추가, DIV 토큰 반환
\=		{return(ASSGN); }
\>		{return(GT);}       // > (Greater Than)
\<		{return(LT);}       // < (Less Than)
\=\=	{return(EQ);}       // == (Equal)
\!\=	{return(NE);}       // != (Not Equal)
\>\=	{return(GE);}       // >= (Greater Than or Equal)
\<\=	{return(LE);}       // <= (Less Than or Equal)
\(		{return('('); }
\)		{return(')'); }
\{		{return('{'); }
\}		{return('}'); }
;		{return(STMTEND); }
main		{return(MAIN); }
if 		{return(IF); }
else 	{return(ELSE); }
{id}		{temp=insertsym(yytext); yylval=MakeNode(ID, temp); return(ID);}
-?{digit}+		{sscanf(yytext, "%d", &temp); yylval=MakeNode(NUM, temp); return(NUM);}
.		{printf("invalid token %s\n", yytext); }
%%

int insertsym(s)
char *s;
{
int i;

	for(i=0;i<maxsym; i++) {
		if(strcmp(s, symtbl[i])==0) return(i);
		}
	if (i<MAXSYM-1) {
		strcpy(symtbl[maxsym],s);
		maxsym++;
		return(maxsym-1);
		}
	else
		{printf("symbol table overflow\n");}
	return(0);
}

yywrap() {}