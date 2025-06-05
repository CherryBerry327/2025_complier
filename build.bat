win_flex --wincompat -o %1lex.c %1.l
win_bison -d -o %1.c %1.y

copy %1lex.c %1
copy %1.c %1
copy %1.h %1
