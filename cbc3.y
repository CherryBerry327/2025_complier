%{
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define DEBUG	0

#define	 MAXSYM	100
#define	 MAXSYMLEN	20
#define	 MAXTSYMLEN	15
#define	 MAXTSYMBOL	MAXSYM/2

#define STMTLIST 500

typedef struct nodeType {
	int token;
	int tokenval;
	struct nodeType *son;
	struct nodeType *brother;
	} Node;

#define YYSTYPE Node*
	
int tsymbolcnt=0;
int errorcnt=0;
int label_count = 0; // new_label() 함수에서 사용하는 변수 선언해주기
FILE *yyin;
FILE *fp;

extern char symtbl[MAXSYM][MAXSYMLEN];
extern int maxsym;
extern int lineno;
extern int yylex(void);

void DFSTree(Node*);
Node * MakeOPTree(int, Node*, Node*);
Node * MakeNode(int, int);
Node * MakeListTree(Node*, Node*);
void codegen(Node* );
void prtcode(int, int);
void yyerror(char *s);

void	dwgen();
int	gentemp();
void	assgnstmt(int, int);
void	numassgn(int, int);
void	addstmt(int, int, int);
void	substmt(int, int, int);
int	insertsym(char *);
%}

%token	MAIN ADD SUB ASSGN ID NUM STMTEND START END ID2
%token  MUL DIV  // 곱하기와 나누기 토큰 선언 추가
%token  IF ELSE  // IF, ELSE 토큰 선언 추가
%token  GT LT EQ NE GE LE // 비교 연산자 토큰 추가

%nonassoc IFX  // "if" without "else" 
%nonassoc ELSE // "else"

%%
program: 	functions 		{ if (errorcnt==0) {codegen($1); dwgen();} }
		;

functions:	functions function
		| function
		;

function:		fun_header fun_body	{$$ = $2; }
		;

fun_header:	MAIN '(' ')' 
		;

fun_body:	'{' dcl_list stmt_list '}'	{$$ = $3; }
		;

dcl_list: 		dcl_list dcl
		| dcl
		;
stmt_list: 		stmt_list stmt 		{$$=MakeListTree($1, $2);}
		| stmt			{$$=MakeListTree(NULL, $1);}
		| error STMTEND		{ errorcnt++; yyerrok;}
		;

stmt: 		expr_stmt
       	 | if_stmt       // (나중에 if문 추가 시 사용)
		 | compound_stmt
       // | while_stmt    // (나중에 while문 추가 시 사용)
        // | for_stmt    // (나중에 for문 추가 시 사용)
        ;
	
compound_stmt: '{' stmt_list '}'
                {
                    $$ = $2; // stmt_list의 AST 노드를 이 규칙의 결과로 합니다.
                             // 또는 블록을 나타내는 새로운 AST 노드 타입을 만들 수도 있습니다.
                }
            | '{' '}' // 빈 블록 {} 도 허용하는 경우
                {
                    $$ = NULL; // 빈 블록은 NULL AST 노드로 표현 (또는 특별한 EMPTY_BLOCK_NODE)
                }
            ;

if_stmt:    IF '(' logical_expr ')' stmt %prec IFX
            {
                $$ = MakeNode(IF, 0);
                $$->son = $3;
                $3->brother = $5;
            }
        |   IF '(' logical_expr ')' stmt ELSE stmt
            {
                $$ = MakeNode(IF, 1);
                $$->son = $3;
                $3->brother = $5;
                $5->brother = $7;
            }
        ;

expr_stmt:	logical_expr STMTEND {$$ = $1;}  // 추가 부분 
		| ID ASSGN logical_expr STMTEND	{ $1->token = ID2; $$=MakeOPTree(ASSGN, $1, $3);}
		;
// 논리 표현식 (현재는 산술 표현식의 최상위 역할) 추가 부분
logical_expr: additive_expr // 비교 연산 없이 산술 표현식만 올 수도 있음
                {
                    $$ = $1;
                }
            | logical_expr GT additive_expr   { $$ = MakeOPTree(GT, $1, $3); }
            | logical_expr LT additive_expr   { $$ = MakeOPTree(LT, $1, $3); }
            | logical_expr EQ additive_expr   { $$ = MakeOPTree(EQ, $1, $3); }
            | logical_expr NE additive_expr   { $$ = MakeOPTree(NE, $1, $3); }
            | logical_expr GE additive_expr   { $$ = MakeOPTree(GE, $1, $3); }
            | logical_expr LE additive_expr   { $$ = MakeOPTree(LE, $1, $3); } 

// 덧셈/뺄셈 표현식 (낮은 우선순위)
additive_expr: 	additive_expr ADD multiplicative_expr	
                    { 
                        $$ = MakeOPTree(ADD, $1, $3); 
                    }
            | additive_expr SUB multiplicative_expr	
                    { 
                        $$ = MakeOPTree(SUB, $1, $3); 
                    }
            | multiplicative_expr 
                    {
                        $$ = $1; 
                    }
            ;
// 곱셈/나눗셈 표현식 (중간 우선순위)
multiplicative_expr: multiplicative_expr MUL primary_expr 
                    { 
                        $$ = MakeOPTree(MUL, $1, $3); 
                    }
                | multiplicative_expr DIV primary_expr 
                    { 
                        $$ = MakeOPTree(DIV, $1, $3); 
                    }
                | primary_expr 
                    {
                        $$ = $1; 
                    }
                ;
// 가장 기본적인 표현식 단위 (가장 높은 우선순위)
primary_expr:		ID		
                    { 
                        $$ = $1; 
                    }
            | NUM		
                    { 
                        $$ = $1; 
                    }
            | '(' additive_expr ')' 
                    { 
                        $$ = $2; 
                    }
            ;

dcl:		;
//f_stmt:		;
//while_stmt:	;

%%

int new_label() {
    return label_count++;
}
int main(int argc, char *argv[]) 
{
	printf("\nsample CBU C compiler v3.0\n");
	printf("(C) Copyright by Jae Sung Lee (jasonlee@cbnu.ac.kr), 2025.\n");
	
	if (argc == 2)
		yyin = fopen(argv[1], "r");
	else {
		printf("Usage: cbc3 inputfile\noutput file is 'a.asm'\n");
		return(0);
		}
		
	fp=fopen("a.asm", "w");
	
	yyparse();
	
	fclose(yyin);
	fclose(fp);

	if (errorcnt==0) 
		{ printf("Successfully compiled. Assembly code is in 'a.asm'.\n");}
}

void yyerror(char *s)
{
	printf("%s (line %d)\n", s, lineno);
}


Node * MakeOPTree(int op, Node* operand1, Node* operand2)
{
Node * newnode;

	newnode = (Node *)malloc(sizeof (Node));
	newnode->token = op;
	newnode->tokenval = op;
	newnode->son = operand1;
	newnode->brother = NULL;
	operand1->brother = operand2;
	return newnode;
}

Node * MakeNode(int token, int operand)
{
Node * newnode;

	newnode = (Node *) malloc(sizeof (Node));
	newnode->token = token;
	newnode->tokenval = operand; 
	newnode->son = newnode->brother = NULL;
	return newnode;
}

Node * MakeListTree(Node* operand1, Node* operand2)
{
Node * newnode;
Node * node;

	if (operand1 == NULL){
		newnode = (Node *)malloc(sizeof (Node));
		newnode->token = newnode-> tokenval = STMTLIST;
		newnode->son = operand2;
		newnode->brother = NULL;
		return newnode;
		}
	else {
		node = operand1->son;
		while (node->brother != NULL) node = node->brother;
		node->brother = operand2;
		return operand1;
		}
}

void codegen(Node * root)
{
	DFSTree(root);
}

// DFSTree 함수 (수정된 최종 제안 버전)
void DFSTree(Node * n) {
    if (n == NULL) return;

    if (n->token == IF) { // IF문 처리
        Node* condition = n->son;
        Node* then_branch = (condition != NULL) ? condition->brother : NULL;
        Node* else_branch = (then_branch != NULL && n->tokenval == 1) ? then_branch->brother : NULL;

        int else_label = new_label();
        int end_if_label = new_label();

        // 1. 조건식 코드 생성 (결과로 0 또는 1이 스택에 남아야 함)
        DFSTree(condition);
        
        // 2. 조건 결과가 0(false)이면 분기
        //    else가 있으면 else_label로, 없으면 end_if_label로 점프
        fprintf(fp, "GOFALSE L%d\n", else_branch != NULL ? else_label : end_if_label);

        // 3. then 절 코드 생성 (조건이 참일 때 실행될 부분)
        DFSTree(then_branch);

        // 4. else 절이 있는 경우에만 GOTO 및 else 관련 코드 생성
        if (else_branch != NULL) {
            fprintf(fp, "GOTO L%d\n", end_if_label); // then 절 실행 후 if 문의 끝으로 점프
            fprintf(fp, "LABEL L%d\n", else_label);  // else 레이블 (GOFALSE의 목적지)
            DFSTree(else_branch);                   // else 절 코드 생성
        }
        // else 절이 없는 경우, GOFALSE가 end_if_label로 바로 점프했거나 (조건 거짓),
        // then 절 실행 후 자연스럽게 아래의 end_if_label로 오게 됨 (조건 참).
        
        // 5. if 문의 끝 레이블
        fprintf(fp, "LABEL L%d\n", end_if_label);
        
        // IF 노드(하나의 문장) 처리 후, 이 IF 문의 형제(다음 문장)를 처리
        DFSTree(n->brother);

    } else if (n->token == GT || n->token == LT || n->token == EQ || 
               n->token == NE || n->token == GE || n->token == LE) {
        
        Node *children_chain_start = n->son; // 여기가 operand1, 이것의 brother가 operand2
        
        DFSTree(children_chain_start); // 왼쪽 및 오른쪽 피연산자 코드 생성
                                       // 스택에는 [..., 왼쪽값, 오른쪽값] 상태가 됨

        int true_path_label = new_label(); 
        int end_compare_label = new_label();

        fprintf(fp, "-\n"); // (왼쪽 피연산자 값 - 오른쪽 피연산자 값)

        if (n->token == GT) { 
            fprintf(fp, "GOPLUS L%d\n", true_path_label);
        } else if (n->token == LT) { 
            fprintf(fp, "GOMINUS L%d\n", true_path_label);
        } else if (n->token == EQ) { 
            fprintf(fp, "GOFALSE L%d\n", true_path_label);
        } else if (n->token == NE) { 
            fprintf(fp, "GOTRUE L%d\n", true_path_label);
        } else if (n->token == GE) { // A >= B  is NOT (A < B)
            int false_condition_label = new_label(); // GE 조건이 거짓일 때(A < B) 점프할 레이블
            fprintf(fp, "GOMINUS L%d\n", false_condition_label); // (A-B) < 0 이면 (즉, GE가 거짓이면) 점프
            fprintf(fp, "PUSH 1\n"); // GE가 참 (A-B >= 0)
            fprintf(fp, "GOTO L%d\n", end_compare_label);
            fprintf(fp, "LABEL L%d\n", false_condition_label);
            fprintf(fp, "PUSH 0\n"); // GE가 거짓 (A-B < 0)
            // end_compare_label은 아래 공통 로직에서 처리하지 않도록 별도 관리
            fprintf(fp, "LABEL L%d\n", end_compare_label);
            // GE/LE는 자체적으로 PUSH 0/1 및 레이블 처리를 완료하므로, 아래 공통 로직을 타지 않음.
            // 하지만 형제 노드 처리는 이 else if 블록 바깥의 최종 else 블록에서 담당하므로 return 불필요.
        } else if (n->token == LE) { // A <= B is NOT (A > B)
            int false_condition_label = new_label(); // LE 조건이 거짓일 때(A > B) 점프할 레이블
            fprintf(fp, "GOPLUS L%d\n", false_condition_label); // (A-B) > 0 이면 (즉, LE가 거짓이면) 점프
            fprintf(fp, "PUSH 1\n"); // LE가 참 (A-B <= 0)
            fprintf(fp, "GOTO L%d\n", end_compare_label);
            fprintf(fp, "LABEL L%d\n", false_condition_label);
            fprintf(fp, "PUSH 0\n"); // LE가 거짓 (A-B > 0)
            fprintf(fp, "LABEL L%d\n", end_compare_label);
        }

        // GT, LT, EQ, NE 에 대한 공통 PUSH 0/1 로직
        if (n->token == GT || n->token == LT || n->token == EQ || n->token == NE) {
            fprintf(fp, "PUSH 0\n"); // 조건부 점프가 일어나지 않았으면 (조건 거짓) PUSH 0
            fprintf(fp, "GOTO L%d\n", end_compare_label);
            fprintf(fp, "LABEL L%d\n", true_path_label);
            fprintf(fp, "PUSH 1\n"); // 조건부 점프가 일어났으면 (조건 참) PUSH 1
            fprintf(fp, "LABEL L%d\n", end_compare_label);
        }
        
        // 비교 연산자 노드는 표현식의 일부이므로, 이 노드의 형제는 상위 구조에서 처리.
        // 여기서 DFSTree(n->brother)를 호출하지 않습니다.

    } else { // 다른 종류의 노드들 처리 (ADD, SUB, MUL, DIV, ID, NUM, ASSGN, STMTLIST 등)
        DFSTree(n->son);
        prtcode(n->token, n->tokenval); // 현재 노드에 대한 코드 생성
        DFSTree(n->brother);           // 형제 노드 처리
    }
}
	


void prtcode(int token, int val)
{
	switch (token) {
	case ID:
		fprintf(fp,"RVALUE %s\n", symtbl[val]);
		break;
	case ID2:
		fprintf(fp, "LVALUE %s\n", symtbl[val]);
		break;
	case NUM:
		fprintf(fp, "PUSH %d\n", val);
		break;
	case ADD:
		fprintf(fp, "+\n");
		break;
	case SUB:
		fprintf(fp, "-\n");
		break;
	 case MUL: // 곱하기 코드 생성 추가
        fprintf(fp, "*\n"); // StackSim의 곱셈 명령어
        break;
    case DIV: // 나누기 코드 생성 추가
        fprintf(fp, "/\n"); // StackSim의 나눗셈 명령어
        break;
	case ASSGN:
		fprintf(fp, ":=\n");
		break;
	case STMTLIST:
	default:
		break;
	};
}


/*
int gentemp()
{
char buffer[MAXTSYMLEN];
char tempsym[MAXSYMLEN]="TTCBU";

	tsymbolcnt++;
	if (tsymbolcnt > MAXTSYMBOL) printf("temp symbol overflow\n");
	itoa(tsymbolcnt, buffer, 10);
	strcat(tempsym, buffer);
	return( insertsym(tempsym) ); // Warning: duplicated symbol is not checked for lazy implementation
}
*/
void dwgen()
{
int i;
	fprintf(fp, "HALT\n");
	fprintf(fp, "$ -- END OF EXECUTION CODE AND START OF VAR DEFINITIONS --\n");

// Warning: this code should be different if variable declaration is supported in the language 
	for(i=0; i<maxsym; i++) 
		fprintf(fp, "DW %s\n", symtbl[i]);
	fprintf(fp, "END\n");
}

