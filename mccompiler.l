%option yylineno

%{ 
#include <string.h>
#include <stdio.h>

#include "aststructures.h"
#include "y.tab.h"

int yycolumn=1;

#define YY_USER_ACTION yylloc.first_line = yylloc.last_line = yylineno; yylloc.first_column = yycolumn; yylloc.last_column = yycolumn+yyleng-1; yycolumn += yyleng;

tree ast;
int eof=0;

int FLAG=0;
int TFLAG=0;
int STFLAG=0;

int column=1;
int line=1;
int begincol,beginline;




char buff[10];

void updateLine(){
	line++;
}

void updateColumn(){
	column+=(int)yyleng;
}


//creates a hard copy of a string
char* createStringCopy(char* s){
	char* res;
	res=(char*)malloc(strlen(s)+1);
	res[0]='\0';
	
	strcpy(res,s);
	return res;
}


//errors
void commentError(){
	printf("Line %d, col %d: unterminated comment\n",beginline,begincol);
	exit(0);
}

void unterminatedCharError(){
	printf("Line %d, col %d: unterminated char constant\n",line,column);
}

void invalidCharError(){
	printf("Line %d, col %d: invalid char constant (%s)\n",line,column,yytext);
}

void unterminatedStringError(){
	printf("Line %d, col %d: unterminated string constant\n",line,column);
}

void invalidStringError(){
	printf("Line %d, col %d: invalid string constant (%s)\n",line,column,yytext);
}

void illegalCharError(){
	printf("Line %d, col %d: illegal character (%s)\n",line,column,yytext);

}
//end errors


%}

%X COMMENT

%{/*numeros,reserveds e ids*/
%}
number							[0-9]
alfa							[a-zA-Z]
reserved "--"|"++"|auto|break|case|const|continue|default|do|double|enum|extern|float|goto|long|register|short|signed|sizeof|static|struct|switch|typedef|union|unsigned|volatile|while
alphanum 					(_|{alfa})(_|{alfa}|{number})* 

%{//sequencias de escape
%}
oescape						[0-7]{1,3}
escape						\\(n|t|\\|'|\"|{oescape})

%{//necessario para detetar strings e chars invalidos
%}
wrongescape					(\\.|\\[8-9]{1,3})

%{//strings nao podem conter aspas, e podem conter plicas, os chars podem conter aspas e nao plicas
%}
charstr						"\t"|" "|{escape}|!|[#-[]|[]-~]
char						\t|" "|{escape}|[!-&]|[(-[]|[]-~]
%{//necessario para detetar strings e chars invalidos
%}
wrongchar					([^\'\n\\]|\\.|\\[0-9]{1,3})

str						{charstr}*
wrongstr					[^\\{char}\"\n]|{wrongescape}
whitespace  			(" "|"\t")+

%%


"/*"		{begincol=column; beginline=line; updateColumn(); BEGIN COMMENT;}

<COMMENT>"*/"	{updateColumn(); BEGIN 0;}
<COMMENT><<EOF>> {commentError();}

<COMMENT>"\n"	{updateLine(); column=1; yycolumn=1;}
<COMMENT>.	{updateColumn();}


"\n"						{updateLine();column=1; yycolumn=1;}

{whitespace}					{updateColumn();}

"&"						{column+=yyleng; if(FLAG==1)printf("AMP\n");return AMP;}
"&&"						{column+=yyleng; if(FLAG==1)printf("AND\n");return AND;}
"="						{column+=yyleng; if(FLAG==1)printf("ASSIGN\n");return ASSIGN;}
"*"						{column+=yyleng; if(FLAG==1)printf("AST\n");return AST;}
"char"						{column+=yyleng; if(FLAG==1)printf("CHAR\n");return CHAR;}
","						{column+=yyleng; if(FLAG==1)printf("COMMA\n");return COMMA;}
"/"						{column+=yyleng; if(FLAG==1)printf("DIV\n");return DIV;}
"else"						{column+=yyleng; if(FLAG==1)printf("ELSE\n");return ELSE;}
"=="						{column+=yyleng; if(FLAG==1)printf("EQ\n");return EQ;}
"for"						{column+=yyleng; if(FLAG==1)printf("FOR\n");return FOR;}
">="						{column+=yyleng; if(FLAG==1)printf("GE\n");return GE;}
">"						{column+=yyleng; if(FLAG==1)printf("GT\n");return GT;}
"if"						{column+=yyleng; if(FLAG==1)printf("IF\n");return IF;}
"int"						{column+=yyleng; if(FLAG==1)printf("INT\n");return INT;}
"{"						{column+=yyleng; if(FLAG==1)printf("LBRACE\n");return LBRACE;}
"<="						{column+=yyleng; if(FLAG==1)printf("LE\n");return LE;}
"("    						{column+=yyleng; if(FLAG==1)printf("LPAR\n");return LPAR;}
"["						{column+=yyleng; if(FLAG==1)printf("LSQ\n");return LSQ;}
"<"						{column+=yyleng; if(FLAG==1)printf("LT\n");return LT;}
"-"						{column+=yyleng; if(FLAG==1)printf("MINUS\n");return MINUS;}
"%"						{column+=yyleng; if(FLAG==1)printf("MOD\n");return MOD;}
"!="						{column+=yyleng; if(FLAG==1)printf("NE\n");return NE;}
"!"						{column+=yyleng; if(FLAG==1)printf("NOT\n");return NOT;}
"||"						{column+=yyleng; if(FLAG==1)printf("OR\n");return OR;}
"+"						{column+=yyleng; if(FLAG==1)printf("PLUS\n");return PLUS;}
"}"						{column+=yyleng; if(FLAG==1)printf("RBRACE\n");return RBRACE;}
"return"					{column+=yyleng; if(FLAG==1)printf("RETURN\n");return RETURN;}
")"    						{column+=yyleng; if(FLAG==1)printf("RPAR\n");return RPAR;}
"]"						{column+=yyleng; if(FLAG==1)printf("RSQ\n");return RSQ;}
";"						{column+=yyleng; if(FLAG==1)printf("SEMI\n");return SEMI;}
"void"						{column+=yyleng; if(FLAG==1)printf("VOID\n");return VOID;}
{reserved}					{column+=yyleng; if(FLAG==1)printf("RESERVED(%s)\n",yytext);yylval.str=createStringCopy(yytext); return RESERVED;}
{alphanum}					{column+=yyleng; if(FLAG==1)printf("ID(%s)\n",yytext);yylval.str=createStringCopy(yytext);return ID;}
{number}+					{column+=yyleng; if(FLAG==1)printf("INTLIT(%s)\n",yytext);yylval.str=createStringCopy(yytext);return INTLIT;}
\"{str}\"					{column+=yyleng; if(FLAG==1)printf("STRLIT(%s)\n",yytext);yylval.str=createStringCopy(yytext);return STRLIT;}


\"({wrongstr})*\"				{invalidStringError();updateColumn();}


\"({wrongstr}|{str})*\\*			{unterminatedStringError();}

'{char}'					{column+=yyleng; if(FLAG==1)printf("CHRLIT(%s)\n",yytext);yylval.str=createStringCopy(yytext);return CHRLIT;}


'{wrongchar}*'					{invalidCharError();updateColumn();}


'{wrongchar}*\\*				{unterminatedCharError();}

<<EOF>>						{eof=1; return EOF;}

.						{illegalCharError();updateColumn();}

%%





int yywrap()
{
return 1;
}



