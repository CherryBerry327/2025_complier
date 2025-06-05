%START AA BB CC
%%
^a                {ECHO; BEGIN AA;}
^b                {ECHO; BEGIN BB;}
^c                {ECHO; BEGIN CC;}
\n                 {ECHO; BEGIN 0;}
<AA>magic        printf("first");
<BB>magic         printf("second");
<CC>magic         printf("third");
%%
main(){ while(1){ yylex();}}
yywrap() {}
