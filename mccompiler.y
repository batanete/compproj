%{
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define DEBUG 0

int yylex(void);

#include "aststructures.h"
#include "astfunctions.h"



extern int line,column,yyleng,eof;
extern int TFLAG,FLAG,STFLAG;
extern tree ast;
extern char* yytext;

#include "semfunctions.h"

int HASERROR=0;


void yyerror(char *s){
	HASERROR=1;
	if(!eof)
		printf ("Line %d, col %d: %s: %s\n", line, column-yyleng, s, yytext);
	else
		printf ("Line %d, col %d: %s: %s\n", line,column,  s,yytext);
}

%}

%locations

%token AMP AND ASSIGN AST CHAR COMMA DIV ELSE EQ FOR RESERVED
%token GE GT IF INT LBRACE LE LPAR LSQ LT MINUS
%token MOD NE NOT OR PLUS RBRACE RETURN RPAR RSQ SEMI VOID

%union
{
	int number;
	char* str;
	char chr;
	nodeptr nodeptr_;
	listptr listptr_;
}

%token <str> INTLIT
%token <str> STRLIT ID
%token <str> CHRLIT

%left COMMA
%left LPAR
%right ASSIGN 
%left OR
%left AND
%left EQ NE
%left GE GT LT LE
%left PLUS MINUS
%left AST DIV MOD
%right UMINUS UNOT UAMP UAST UPLUS

%left LSQ

%nonassoc STAT
%nonassoc DECL

%nonassoc IFSTAT
%nonassoc ELSE



%type<nodeptr_> prog Expr Statement ExprOrNull TypeSpec FunctionDefinition FunctionDeclaration FunctionBody

%type<nodeptr_> ParameterList ParameterDeclaration IfElse ExprNoComma StatementOrError

%type<listptr_> PointerList Declarations FunctionDeclarator ProgDeclarations ParameterDeclarations Declaration Declarator
%type<listptr_> StatList CommaExpr OneOrMoreStatements



%%


prog:
 ProgDeclarations			{if(!HASERROR){ast=createNode("Program",$1,NULL); if(TFLAG==1)printTree(ast,0); $$=ast;}if(!HASERROR && TFLAG)deleteTree(ast);}
;



/*declarations of variables and functions on the top level of the program.*/
ProgDeclarations:
  FunctionDefinition				{if(!HASERROR){listptr res=createList(); addNode(res,$1); $$=res;}}
| FunctionDeclaration				{if(!HASERROR){listptr res=createList(); addNode(res,$1); $$=res;}}
| Declaration					{if(!HASERROR)$$=$1;}
| ProgDeclarations FunctionDefinition		{if(!HASERROR){listptr res=createList(); addNode(res,$2); res=concatLists($1,res); $$=res;}}
| ProgDeclarations FunctionDeclaration		{if(!HASERROR){listptr res=createList(); addNode(res,$2); res=concatLists($1,res); $$=res;}}
| ProgDeclarations Declaration			{if(!HASERROR)$$=concatLists($1,$2);}
;


/*funcoes sem corpo*/
FunctionDeclaration :
 TypeSpec FunctionDeclarator SEMI		{if(!HASERROR){listptr lp=createList(); addNode(lp,$1); lp=concatLists(lp,$2); 
						$$=createNode("FuncDeclaration",lp,NULL);}}
;

/*nao podemos ter parametros vazios*/
FunctionDefinition:
 TypeSpec FunctionDeclarator FunctionBody	{if(!HASERROR){listptr lp=createList(); addNode(lp,$1); lp=concatLists(lp,$2); 
						addNode(lp,$3); $$=createNode("FuncDefinition",lp,NULL);}}
;


FunctionBody:
 	LBRACE Declarations RBRACE 		{if(!HASERROR)$$=createNode("FuncBody",$2,NULL);}
|  	LBRACE OneOrMoreStatements RBRACE    	{if(!HASERROR){$$=createNode("FuncBody",$2,NULL);}}

|  	LBRACE Declarations OneOrMoreStatements RBRACE 	{if(!HASERROR){listptr lp=$2; lp=concatLists(lp,$3); $$=createNode("FuncBody",lp,NULL);}}

|       LBRACE RBRACE				{if(!HASERROR)$$=createNode("FuncBody",NULL,NULL);}

|	WrongFunction				{}
;

Declarations:
	Declaration				{if(!HASERROR)$$=$1;}
|	Declaration Declarations		{if(!HASERROR)$$=concatLists($1,$2);}
;



FunctionDeclarator:
	PointerList ID LPAR ParameterList RPAR	{if(!HASERROR){listptr lp=createList(); lp=concatLists(lp,$1);nodeptr np=createNode("Id",NULL,$2); addNode(lp,np); addNode(lp,$4); $$=lp;setLineColumn(np,@2.last_line,@2.first_column);}}
;

ParameterList:
	ParameterDeclarations	{if(!HASERROR){listptr lp=$1;  $$=createNode("ParamList",lp,NULL);}}
;


ParameterDeclarations:
   ParameterDeclaration					{if(!HASERROR){listptr lp=createList(); addNode(lp,$1); $$=lp;}}
 | ParameterDeclarations COMMA ParameterDeclaration	{if(!HASERROR){listptr lp=$1; addNode(lp,$3); $$=lp;}}
;

ParameterDeclaration:
  TypeSpec PointerList				{if(!HASERROR){listptr lp=createList(); addNode(lp,$1); lp=concatLists(lp,$2); $$=createNode("ParamDeclaration",lp,NULL);}}
 | TypeSpec PointerList	ID			{if(!HASERROR){listptr lp=createList(); addNode(lp,$1); lp=concatLists(lp,$2); addNode(lp,createNode("Id",NULL,$3)); $$=createNode("ParamDeclaration",lp,NULL);setLineColumn($$,@3.last_line,@3.first_column);
$$->columntype=@1.first_column; $$->linetype=@1.last_line;
}}
;


TypeSpec :
  CHAR  {if(!HASERROR){$$=createNode("Char",NULL,NULL);setLineColumn($$,@1.last_line,@1.first_column);}}
 | INT  {if(!HASERROR){$$=createNode("Int",NULL,NULL);setLineColumn($$,@1.last_line,@1.first_column);}}
 | VOID	{if(!HASERROR){$$=createNode("Void",NULL,NULL);setLineColumn($$,@1.last_line,@1.first_column);}}
;


Declaration:
  TypeSpec Declarator SEMI			{if(!HASERROR){$$=createDeclarationList($1,$2);
nodeptr aux;
aux=$$->first;

while(aux){
	aux->linetype=@1.last_line;
	aux->columntype=@1.first_column;
	aux=aux->next;
}

}}
| WrongDeclarationOrStatement			{}
;

WrongDeclarationOrStatement:
 error SEMI					{}
;

WrongFunction:
LBRACE error RBRACE				{}
;


Declarator:
/*Declarator can be of array, of pointer(or pointer to pointer) type, both or none.*/
   PointerList ID LSQ INTLIT RSQ			{
				if(!HASERROR){
					listptr lp=$1;
					listptr lparr=createList();
					addNode(lparr,createNode("Id",NULL,$2)); addNode(lparr,createNode("IntLit",NULL,$4));
					
					lp=concatLists(lp,lparr);
					nodeptr res=createNode("DeclaratorArray",lp,NULL);
					setLineColumn(res,@2.last_line,@2.first_column);
					listptr lpres=createList(); addNode(lpres,res); $$=lpres;
				}
				}
 | PointerList ID 					{
							if(!HASERROR){
								listptr lp=$1; addNode(lp,createNode("Id",NULL,$2));
								nodeptr res=createNode("Declarator",lp,NULL);
								setLineColumn(res,@2.last_line,@2.first_column);
								listptr lpres=createList(); addNode(lpres,res); $$=lpres;
								
							}
							}
 | Declarator COMMA PointerList ID LSQ INTLIT RSQ	{
							if(!HASERROR){
								listptr lp=$3;
								listptr lparr=createList();
								addNode(lparr,createNode("Id",NULL,$4)); addNode(lparr,createNode("IntLit",NULL,$6));		
								lp=concatLists(lp,lparr);
								nodeptr res=createNode("DeclaratorArray",lp,NULL);
								setLineColumn(res,@4.last_line,@4.first_column);
								listptr lpres=createList(); addNode(lpres,res); $$=concatLists($1,lpres);
							}
							}
 | Declarator COMMA PointerList ID 			{if(!HASERROR){
								listptr lplast=$1; 
								listptr lp=$3; addNode(lp,createNode("Id",NULL,$4));
								
								nodeptr res=createNode("Declarator",lp,NULL);
								setLineColumn(res,@4.last_line,@4.first_column);
								listptr lpres=createList(); addNode(lpres,res); $$=concatLists(lplast,lpres);
							}
							}					
;

/*List of zero or more pointers.*/
PointerList:
						{if(!HASERROR) $$=createList();}
|	PointerList AST				{if(!HASERROR) {listptr res=$1; nodeptr np=createNode("Pointer",NULL,NULL); setLineColumn(np,@2.last_line,@2.first_column); addNode(res,np); $$=res;}}
;

/*Normal expressions plus comma ones.*/
Expr:
  ExprNoComma					{if(!HASERROR){$$=$1;}}
| Expr COMMA Expr				{if(!HASERROR){listptr lp=createList(); addNode(lp,$1); addNode(lp,$3); $$=createNode("Comma",lp,NULL);setLineColumn($$,@2.last_line,@2.first_column);}}
;


/*Needed because of function calls comma ambiguities.*/
ExprNoComma:
	ExprNoComma AST ExprNoComma		{if(!HASERROR){ listptr lp=createList(); addNode(lp,$1); addNode(lp,$3); $$=createNode("Mul",lp,NULL); setLineColumn($$,@2.last_line,@2.first_column);}  }
 |	ExprNoComma DIV ExprNoComma		{if(!HASERROR) {listptr lp=createList(); addNode(lp,$1); addNode(lp,$3); $$=createNode("Div",lp,NULL); setLineColumn($$,@2.last_line,@2.first_column);}}
 |	ExprNoComma MOD ExprNoComma		{if(!HASERROR){listptr lp=createList(); addNode(lp,$1); addNode(lp,$3); $$=createNode("Mod",lp,NULL);setLineColumn($$,@2.last_line,@2.first_column);}}

 |	ExprNoComma PLUS ExprNoComma		{if(!HASERROR){listptr lp=createList(); addNode(lp,$1); addNode(lp,$3); $$=createNode("Add",lp,NULL);setLineColumn($$,@2.last_line,@2.first_column);}}
 |	ExprNoComma MINUS ExprNoComma		{if(!HASERROR){listptr lp=createList(); addNode(lp,$1); addNode(lp,$3); $$=createNode("Sub",lp,NULL);setLineColumn($$,@2.last_line,@2.first_column);}}

 |	ExprNoComma GT ExprNoComma		{if(!HASERROR){listptr lp=createList(); addNode(lp,$1); addNode(lp,$3); $$=createNode("Gt",lp,NULL);setLineColumn($$,@2.last_line,@2.first_column);}}
 |	ExprNoComma LT ExprNoComma		{if(!HASERROR){listptr lp=createList(); addNode(lp,$1); addNode(lp,$3); $$=createNode("Lt",lp,NULL);setLineColumn($$,@2.last_line,@2.first_column);}}
 |	ExprNoComma GE ExprNoComma		{if(!HASERROR){listptr lp=createList(); addNode(lp,$1); addNode(lp,$3); $$=createNode("Ge",lp,NULL);setLineColumn($$,@2.last_line,@2.first_column);}}
 |	ExprNoComma LE ExprNoComma		{if(!HASERROR){listptr lp=createList(); addNode(lp,$1); addNode(lp,$3); $$=createNode("Le",lp,NULL);setLineColumn($$,@2.last_line,@2.first_column);}}
 
 |	ExprNoComma EQ ExprNoComma		{if(!HASERROR){listptr lp=createList(); addNode(lp,$1); addNode(lp,$3); $$=createNode("Eq",lp,NULL);setLineColumn($$,@2.last_line,@2.first_column);}}
 |	ExprNoComma NE ExprNoComma		{if(!HASERROR){listptr lp=createList(); addNode(lp,$1); addNode(lp,$3); $$=createNode("Ne",lp,NULL);setLineColumn($$,@2.last_line,@2.first_column);}}
 
 |	ExprNoComma AND ExprNoComma		{if(!HASERROR){listptr lp=createList(); addNode(lp,$1); addNode(lp,$3); $$=createNode("And",lp,NULL);setLineColumn($$,@2.last_line,@2.first_column);}}

 |	ExprNoComma OR ExprNoComma		{if(!HASERROR){listptr lp=createList(); addNode(lp,$1); addNode(lp,$3); $$=createNode("Or",lp,NULL);setLineColumn($$,@2.last_line,@2.first_column);}}

 |	ExprNoComma ASSIGN ExprNoComma		{if(!HASERROR){listptr lp=createList(); addNode(lp,$1); addNode(lp,$3); $$=createNode("Store",lp,NULL);setLineColumn($$,@2.last_line,@2.first_column);}}


 | LPAR Expr RPAR  				{if(!HASERROR) $$=$2;}


 
 
 | ExprNoComma LSQ Expr RSQ{if(!HASERROR){listptr lpadd=createList(); addNode(lpadd,$1); addNode(lpadd,$3);
				nodeptr add=createNode("Add",lpadd,NULL);
				listptr lp=createList();
				addNode(lp,add);
				$$=createNode("Deref",lp,NULL);}}
 
 | ID LPAR RPAR 			{if(!HASERROR){
						listptr lp=createList(); addNode(lp,createNode("Id",NULL,$1)); $$=createNode("Call",lp,NULL);setLineColumn($$,@1.last_line,@1.first_column);}}
 | ID LPAR ExprNoComma CommaExpr RPAR 	{if(!HASERROR){listptr lp=createList(); addNode(lp,createNode("Id",NULL,$1)); addNode(lp,$3); lp=concatLists(lp,$4); $$=createNode("Call",lp,NULL);setLineColumn($$,@1.last_line,@1.first_column);}}
 
/*Unary operations are done after the binary ones.*/
 | MINUS ExprNoComma %prec UMINUS	{if(!HASERROR){listptr lp=createList(); addNode(lp,$2); $$=createNode("Minus",lp,NULL);setLineColumn($$,@1.last_line,@1.first_column);}}
 | PLUS ExprNoComma %prec UPLUS		{if(!HASERROR){listptr lp=createList(); addNode(lp,$2); $$=createNode("Plus",lp,NULL);setLineColumn($$,@1.last_line,@1.first_column);}}
 | AST ExprNoComma  %prec UAST		{if(!HASERROR){listptr lp=createList(); addNode(lp,$2); $$=createNode("Deref",lp,NULL);setLineColumn($$,@1.last_line,@1.first_column);}}
 | AMP ExprNoComma  %prec UAMP		{if(!HASERROR){listptr lp=createList(); addNode(lp,$2); $$=createNode("Addr",lp,NULL);setLineColumn($$,@1.last_line,@1.first_column);}}
 | NOT ExprNoComma  %prec UNOT		{if(!HASERROR){listptr lp=createList(); addNode(lp,$2); $$=createNode("Not",lp,NULL);setLineColumn($$,@1.last_line,@1.first_column);}}
  
 | ID 				{{if(!HASERROR)$$=createNode("Id",NULL,$1);setLineColumn($$,@1.last_line,@1.first_column);}}
 | INTLIT 			{if(!HASERROR)$$=createNode("IntLit",NULL,$1);setLineColumn($$,@1.last_line,@1.first_column);}
 | CHRLIT 			{if(!HASERROR)$$=createNode("ChrLit",NULL,$1);setLineColumn($$,@1.last_line,@1.first_column);}
 | STRLIT  			{if(!HASERROR)$$=createNode("StrLit",NULL,$1);setLineColumn($$,@1.last_line,@1.first_column);}

 /*Errors.*/
 | LPAR error RPAR		{}
 | ID LPAR error RPAR		{}
 ;


/*Used in function calls.*/
 CommaExpr:
   				{if(!HASERROR){listptr lp=createList();  $$=lp;}}
 | CommaExpr COMMA ExprNoComma 	{if(!HASERROR){listptr lp=$1; addNode(lp,$3);  $$=lp;}}
;

/*Needed since SEMI is a valid statement in mC.*/
ExprOrNull:
				{if(!HASERROR) $$=NULL;}
 | Expr				{if(!HASERROR) $$=$1;}
 ;

/*At least one correct statement*/
OneOrMoreStatements:
  Statement					{if(!HASERROR){listptr lp=createList(); if($1)addNode(lp,$1); $$=lp;}}
| OneOrMoreStatements StatementOrError		{if(!HASERROR){listptr lp=$1; if($2)addNode(lp,$2); $$=lp;}}
;

 StatList:
	/*concatenamos as listas de filhos das statlists.*/
  StatList StatementOrError {if(!HASERROR){listptr lp=$1; if($2!=NULL)addNode(lp,$2); $$=lp;}}
 |StatementOrError StatementOrError {if(!HASERROR){listptr lp=createList(); if($1!=NULL)addNode(lp,$1); if($2)addNode(lp,$2); $$=lp;}}
 ;


 StatementOrError:
  error SEMI					{}
| Statement					{if(!HASERROR)$$=$1;}
;

 Statement:
   ExprOrNull SEMI				{if(!HASERROR)$$=$1;}
 | IfElse					{if(!HASERROR)$$=$1;}
 | LBRACE RBRACE				{if(!HASERROR)$$=NULL;}
 | LBRACE error RBRACE				{}
 | LBRACE StatList RBRACE 			{if(!HASERROR){//can only be statlist if it has at least 2 elements
							if($2->nelements>1)
								$$=createNode("StatList",$2,NULL);
							else if($2->nelements==1)
								$$=$2->first;
							else if($2->nelements==0)
								$$=NULL;
						}}


 | LBRACE StatementOrError RBRACE 		{if(!HASERROR)$$=$2;}
 | FOR LPAR ExprOrNull SEMI ExprOrNull SEMI ExprOrNull RPAR StatementOrError {if(!HASERROR){listptr lp=createList(); addNode(lp,$3);
								      addNode(lp,$5);
                                                                      addNode(lp,$7);
								      addNode(lp,$9);
								      $$=createNode("For",lp,NULL);}}
 | RETURN Expr SEMI	{if(!HASERROR){listptr lp=createList(); addNode(lp,$2); $$=createNode("Return",lp,NULL);}}
 | RETURN SEMI		{if(!HASERROR){$$=createNode("Return",NULL,NULL);}}
 ;

/*temos de dar prioridade ao operador ELSE, para evitar o problema do dangling else.*/
IfElse:
  IF LPAR Expr RPAR StatementOrError %prec IFSTAT	{if(!HASERROR){listptr lp=createList(); addNode(lp,$3); addNode(lp,$5); 
$$=createNode("If",lp,NULL);}}

| IF LPAR Expr RPAR StatementOrError ELSE StatementOrError 	{if(!HASERROR){listptr lp=createList(); addNode(lp,$3); addNode(lp,$5); 
addNode(lp,$7); $$=createNode("If",lp,NULL);}}
;


/* A funcao main() esta do lado do lex */
%%




int main(int argc,char** argv)
{
	if(argc>=2){
		//tokens e erros de sintaxe
		if(strcmp(argv[1],"-l")==0)
			FLAG=1;
		//apenas imprime erros de sintaxe
		else if(strcmp(argv[1],"-1")==0)
			FLAG=2;
		else if(strcmp(argv[1],"-t")==0)
			TFLAG=1;
		else if(strcmp(argv[1],"-2")==0)
			TFLAG=2;
		else if(strcmp(argv[1],"-s")==0)
			STFLAG=1;
		
	}
	yyparse();
	
	//proceed to semantics if there are no errors and the arguments are appropriate to do it.
	if(!HASERROR && !FLAG && !TFLAG && STFLAG){
		getSymbolTables(1);
	}
	else if(!FLAG && !TFLAG)
		getSymbolTables(0);
	
	
	return 0;
}

