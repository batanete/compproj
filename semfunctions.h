#ifndef SEMANTICS
#define SEMANTICS

typedef struct symtablestr* stptr;
typedef struct symtableliststr* stlptr;
//typedef struct symbolstr* symptr;
typedef struct symliststr* symlistptr;

enum vartype{Int,Char,Void};
enum stype{Func,Var,Array};

typedef struct symbolstr{
	char* id;
	enum vartype vtype;
	enum stype type;
	
	//1=*, 2=**,etc
	int npointer;
	
	//only used in arrays
	int nelements;
	
	//only used in functions. if isdef=1, it's a definition and not a declaration.
	symlistptr params; int isdef;
	int line,column;
	
	int linetype,columntype;
	
	int isparam;
	symptr next;
}symbol;

typedef struct symliststr{
	symptr first;
	int nelements;
}symlist;

//symbol table
typedef struct symtablestr{
	char* id;
	int npointer;
	
	symlistptr symbols;
	stptr next;
	
	//says whether entry to st list is fake. if it is we just ignore it when printing.
	int isfake;
	
	
	enum vartype returntype;
}symtable;

//symbol table list
typedef struct symtableliststr{
	stptr first;
	int nelements;
}symtablelist;


int checkVarDecl(symptr,stptr);
int checkArrayDecl(symptr,stptr);
int checkFunctionDecl(symptr);

//create empty symbol list
symlistptr createSymList(){
	symlistptr res;
	res=(symlistptr)malloc(sizeof(symlist));
	res->first=NULL;
	res->nelements=0;
	return res;
}


//global scope symbol table
stptr globalst;

//global scope symbol table
stlptr globalstlist;

//creates a symbol that represents a parameter of a function.
symptr newParam(enum vartype vt,int npointer){
	symptr res;
	res=(symptr)malloc(sizeof(symbol));
	res->npointer=npointer;
	res->vtype=vt;
	res->isparam=1;
	
	return res;
}

//creates a dynamic copy of the string given using malloc.
char* newString(char* s){
	char* res;
	res=(char*) malloc(strlen(s)+1);
	res[0]='\0';
	strcpy(res,s);
	
	return res;
}


char* vartypeToStr(enum vartype vt){
	if(vt==Int){
		return newString("int");	
	}
	else if(vt==Char){
		return newString("char");
	}
	else return newString("void");
}

//add symbol to symbol list
void addSymbol(symlistptr sl,symptr sym){
	symptr aux;
	int i;
	
	if(sl->nelements==0){
		sl->nelements=1;
		sl->first=sym;
		sym->next=NULL;
		return;
	}
	
	aux=sl->first;
	
	for(i=0;i<sl->nelements-1;i++)
		aux=aux->next;
	
	aux->next=sym;
	sym->next=NULL;
	sl->nelements++;
}

//create new symbol table list
stlptr newSTL(){
	stlptr res;
	res=(stlptr)malloc(sizeof(symtablelist));
	res->nelements=0; res->first=NULL;
	return res;
}

//add symbol table to the list
void addST(stlptr stl,stptr st){
	int i;
	stptr curr,last;
	
	if(stl->nelements==0){
		stl->first=st;
		stl->nelements=1;
		return;
	}
	
	last=stl->first;
	curr=stl->first->next;
	for(i=1;i<stl->nelements;i++){
		//function redeclared/defined
		if(strcmp(curr->id,st->id)==0){
			//TODO:limpar memoria de curr,verificar o tipo/parametros da funcao
			
			if(st->isfake==1 && curr->isfake==0)
				return;
			
			last->next=st;
			st->next=curr->next;
			return;
		}
		last=curr;
		curr=curr->next;
	}
	
	last->next=st;
	stl->nelements++;
}

//creates variable symbol from declaration tree node
symptr createVarSymbol(nodeptr np,int isparam){
	symptr res;
	nodeptr aux;
	res=(symptr)malloc(sizeof(symbol));
	res->npointer=0;
	res->type=Var;
	res->isparam=isparam;
	res->column=np->column;
	res->line=np->line;
	
	res->columntype=np->columntype;
	res->linetype=np->linetype;
	
	aux=np->children->first;
	if(strcmp(aux->type,"Int")==0){
		res->vtype=Int;
	}
	else if(strcmp(aux->type,"Char")==0){
		res->vtype=Char;
	}
	else if(strcmp(aux->type,"Void")==0){
		res->vtype=Void;
	}
	
	aux=aux->next;
	
	//doesn't have id, is a function parameter without id.
	if(aux==NULL){
		res->id=NULL;
		return res;
	}
	
	while(strcmp(aux->type,"Id")!=0){
		res->npointer++;
		aux=aux->next;
		
		//doesn't have id, is a function parameter without id.
		if(aux==NULL){
			res->id=NULL;
			return res;
		}
		
	}
	
	
	res->id=(char*)malloc(strlen(aux->lit)+1);
	res->id[0]='\0';
	strcpy(res->id,aux->lit);
	
	return res;
}

//creates function symbol from function declaration or definition
symptr createFuncSymbol(nodeptr np){
	symptr res;
	symptr symaux;
	nodeptr aux;
	int tam,i;
	
	res=(symptr)malloc(sizeof(symbol));
	res->npointer=0;
	res->type=Func;
	
	if(strcmp(np->type,"FuncDefinition")==0)
		res->isdef=1;
	else res->isdef=0;
	
	aux=np->children->first;
	if(strcmp(aux->type,"Int")==0){
		res->vtype=Int;
	}
	else if(strcmp(aux->type,"Char")==0){
		res->vtype=Char;
	}
	else if(strcmp(aux->type,"Void")==0){
		res->vtype=Void;
	}
	
	aux=aux->next;
	
	while(strcmp(aux->type,"Id")!=0){
		res->npointer++;
		aux=aux->next;
	}
	res->line=aux->line;
	res->column=aux->column;
	
	
	res->id=(char*)malloc(strlen(aux->lit)+1);
	res->id[0]='\0';
	strcpy(res->id,aux->lit);	
	
	while(strcmp(aux->type,"ParamList")!=0)
		aux=aux->next;
	
	tam=aux->children->nelements;
	aux=aux->children->first;
	res->params=createSymList();
	
	
	//if parameter is just void, params will be an empty list
	if(tam==1){
		if(aux->children->nelements==1 && strcmp(aux->children->first->type,"Void")==0){
			return res;
		}
	}
	
	for(i=0;i<tam;i++){
		symaux=createVarSymbol(aux,0);
		
		addSymbol(res->params,symaux);
		aux=aux->next;
	}
	
	return res;
}


//creates function symbol from Array declaration tree node
symptr createArraySymbol(nodeptr np){
	symptr res;
	nodeptr aux;
	res=(symptr)malloc(sizeof(symbol));
	res->npointer=0;
	res->type=Array;
	res->line=np->line;
	res->column=np->column;
	
	res->linetype=np->linetype;
	res->columntype=np->columntype;
	
	aux=np->children->first;
	if(strcmp(aux->type,"Int")==0){
		res->vtype=Int;
	}
	else if(strcmp(aux->type,"Char")==0){
		res->vtype=Char;
	}
	else if(strcmp(aux->type,"Void")==0){
		res->vtype=Void;
	}
	
	aux=aux->next;
	while(strcmp(aux->type,"Id")!=0){
		res->npointer++;
		aux=aux->next;
	}
	
	
	res->id=(char*)malloc(strlen(aux->lit)+1);
	res->id[0]='\0';
	strcpy(res->id,aux->lit);
	
	aux=aux->next;
	
	//number of elements of array
	//octal form
	if(aux->lit[0]=='0')
		sscanf(aux->lit,"%o",&(res->nelements));
	else
		res->nelements=atoi(aux->lit);
	
	return res;
}



//adds symbol to the given symbol table. Returns 1 on success, 0 on fail
int addSymbolToST(stptr st,symptr sp){
	symptr aux;
	int i;
	if(st->symbols->nelements==0){
		st->symbols->first=sp;
		st->symbols->nelements=1;
		sp->next=NULL;
		return 1;
	}
	
	
	aux=st->symbols->first;
	
	if(strcmp(aux->id,sp->id)==0)
		return 0;
	
	for(i=0;i<st->symbols->nelements-1;i++){
		aux=aux->next;
		
		//if we already have this id, the only way there is no error is if this one is a funcdecl and the 
		//one we want to add is a funcdef.
		//printf("aux: %s, sp: %s\n",aux->id,sp->id);
		if(strcmp(aux->id,sp->id)==0){
			//printf("nao sou o primeiro\n");
			
			//if(!(sp->isdef) || (aux->isdef) || (!aux->vtype==Func) || (!sp->vtype==Func))
				return 0;
			//return 1;
		}
	}
	
	aux->next=sp;
	st->symbols->nelements++;
	sp->next=NULL;
	return 1;
}


//prints var type compared to string
void printVtype(enum vartype v){
	if(v==Int)
		printf("int");
	else if(v==Char)
		printf("char");
	else if(v==Void)
		printf("void");
	else
		printf("ERROR ON FUNCTION PRINTVTYPE\n");
}

//prints a symbol on screen.
void printSymbol(symptr sym){
	char* vtype;
	int i,j;
	symptr auxsym;
	
	switch(sym->vtype){
		case(Int):
			vtype="int";
			break;
		case(Char):
			vtype="char";
			break;
		case(Void):
			vtype="void";
			break;
		default:
			printf("ERROR IN FUNCTION PRINTSYMBOL.");
			break;
	}
	
	//variables
	if(sym->type==Var){
		printf("%s\t%s",sym->id,vtype);
		
		for(i=0;i<sym->npointer;i++)
			printf("*");
		
		if(sym->isparam)
			printf("\tparam");
		printf("\n");
	}
	//arrays
	else if(sym->type==Array){
		printf("%s\t%s",sym->id,vtype);
		
		for(i=0;i<sym->npointer;i++)
			printf("*");
		
		printf("[%d]\n",sym->nelements);
		
	}
	else if(sym->type==Func){
		printf("%s\t%s",sym->id,vtype);
		
		for(i=0;i<sym->npointer;i++)
			printf("*");
		
		printf("(");
		
		//empty parameter list
		if(sym->params->nelements==0){
			printf("void)\n");
			return;
		}
		
		auxsym=sym->params->first;
		
		//first parameter
		printVtype(auxsym->vtype);
		for(j=0;j<auxsym->npointer;j++)
			printf("*");
		
		//remaining parameters
		for(i=1;i<sym->params->nelements;i++){
				auxsym=auxsym->next;
				printf(",");
				printVtype(auxsym->vtype);
				for(j=0;j<auxsym->npointer;j++)
					printf("*");
		}
		
		printf(")\n");
		
	}
}

//prints symbol table on the screen
void printST(stptr st){
	int i;
	symptr aux;
	//global st
	if(st->id==NULL)
		printf("===== Global Symbol Table =====\n");
	
	//function st
	else{
		printf("===== Function %s Symbol Table =====\n",st->id);

		printf("return\t");
		printVtype(st->returntype);
		
		for(i=0;i<st->npointer;i++)
			printf("*");
		printf("\n");
		
	}
	
	aux=st->symbols->first;
	for(i=0;i<st->symbols->nelements;i++){
		printSymbol(aux);
		aux=aux->next;
	}
}


//print all symbol tables from a given list.
void printSTList(stlptr stl){
	int i;
	stptr aux;
	
	aux=stl->first;
	
	if(!aux)
		return;
	
	for(i=0;i<stl->nelements;i++){
		if(aux->isfake==0){
			printST(aux);
		
			printf("\n");
		}
		aux=aux->next;
	}
}

//returns vtype from string
enum vartype getVType(char* type){
	if(strcmp(type,"Int")==0){
		return Int;
	}
	else if(strcmp(type,"Char")==0){
		return Char;
	}
	else{
		return Void;
	}	
}


//searches for a symbol in a symbol table, and returns a pointer to it if it exists, or NULL
symptr searchSymbol(stptr st,char* id){
	int i;
	symptr symaux;
	
	if(st->symbols->nelements==0)
		return NULL;
	
	symaux=st->symbols->first;
	
	for(i=0;i<st->symbols->nelements;i++){
		if(strcmp(symaux->id,id)==0)
			return symaux;
		
		symaux=symaux->next;
	}
	return NULL;
}


int repetedParams(symptr func){
	int i,j;
	int res=0;
	symptr aux1,aux2,aux3;
	
	if(func->params->nelements<2)
		return 0;
	
	aux1=func->params->first;
	aux2=aux1->next;
	aux3=aux2;
	for(i=0;i<func->params->nelements-1;i++){
		
		for(j=i;j<func->params->nelements-1;j++){
			
			if(aux1->id==NULL || aux2->id==NULL){
				aux2=aux2->next;
				continue;
			}
			
			if(strcmp(aux1->id,aux2->id)==0){
				printf("Line %d, col %d: Symbol %s already defined\n",aux2->line,aux2->column,aux2->id);
				res=1;
				break;
			}
			if(aux2->next!=NULL)
				aux2=aux2->next;
		}
		aux1=aux1->next;
		aux2=aux3->next;
		aux3=aux3->next;
	}
	return res;
}

//checks if two function declarations are equivalent(same return type and parameters)
int compareFuncDecls(symptr f1,symptr f2){
	symptr aux1,aux2;
	
	if(f1->vtype!=f2->vtype){
		return 0;
	}
	
	if(f1->npointer!=f2->npointer){
		return 0;
	}
	
	if(f1->params->nelements!=f2->params->nelements){
		
		return 0;
	}
	
	if(f1->params->nelements==0){
		
		return 1;
	}
	
	
	aux1=f1->params->first; aux2=f2->params->first;
	
	while(aux1){
		if(aux1->vtype!=aux2->vtype)
			return 0;
		if(aux1->npointer!=aux2->npointer)
			return 0;
		aux1=aux1->next;
		aux2=aux2->next;
	}
	return 1;
}




//determines if a function we are trying to define already is defined as anything but a single func decl with
//the same type and arguments.returns 1 in case it can be defined without issues.
int canFunctionBeDefined(symptr func){
	symptr aux;
	char* id;
	id=func->id;

	if(globalst->symbols->nelements==0){
		return 1;
	}
	
	aux=globalst->symbols->first;
	
	while(aux){

		if(strcmp(aux->id,id)==0){
			
			if(aux->type!=Func){
				return 0;}
				
			else if(aux->isdef==1){
				if(compareFuncDecls(func,aux)){
					printf("Line %d, col %d: Symbol %s already defined\n",func->line,func->column,func->id);
					return 1;
				}
				else{
					return 0;
				}
				
				return 1;}
			//in case function is already declared, and we're trying to define it
			else{
				if(!compareFuncDecls(func,aux)){
					return 0;}
				
				aux->isdef=1;
				
			}
		}
		aux=aux->next;
	}
	
	

	return 1;
}

int checkVarDecl(symptr sym,stptr func){
	symptr symaux;
	
	//printf("ola\n");
	//void declaration(error)
	if(sym->npointer==0 && sym->vtype==Void){
		printf("Line %d, col %d: Invalid use of void type in declaration\n",sym->linetype,sym->columntype);
		return 0;
	}
	
	symaux=searchSymbol(func,sym->id);
	
	if(symaux!=NULL){
		
		if(func!=globalst){
			if(symaux->type==sym->type && symaux->vtype==sym->vtype && symaux->npointer==sym->npointer)
				printf("Line %d, col %d: Symbol %s already defined\n",sym->line,sym->column,sym->id);
			else{
				printf("Line %d, col %d: Conflicting types (got ",sym->line,sym->column);
				printConflict(sym);
				printf(", expected ");
				printConflict(symaux);
				printf(")\n");
			}
			return 0;
		}
		
		if(symaux->npointer!=sym->npointer || symaux->type!=sym->type || symaux->vtype!=sym->vtype){
			printf("Line %d, col %d: Conflicting types (got ",sym->line,sym->column);
			printConflict(sym);
			printf(", expected ");
			printConflict(symaux);
			printf(")\n");
			return 0;
		}
	}
	
	return 1;
	//TODO
}

int checkArrayDecl(symptr sym,stptr func){
	symptr symaux;
	
	
	
	//printf("ola\n");
	//void declaration(error)
	if(sym->npointer==0 && sym->vtype==Void){
		printf("Line %d, col %d: Invalid use of void type in declaration\n",sym->linetype,sym->columntype);
		return 0;
	}
	
	symaux=searchSymbol(func,sym->id);
	
	if(symaux!=NULL){
		
		if(func!=globalst){
			if(symaux->type==sym->type && symaux->vtype==sym->vtype && symaux->npointer==sym->npointer
			&& symaux->nelements==sym->nelements){
				
				
				printf("Line %d, col %d: Symbol %s already defined\n",sym->line,sym->column,sym->id);
			}
			else{
				printf("Line %d, col %d: Conflicting types (got ",sym->line,sym->column);
				printConflict(sym);
				printf(", expected ");
				printConflict(symaux);
				printf(")\n");
			}
			return 0;
		}
		
		
		if(symaux->npointer!=sym->npointer || symaux->type!=sym->type || 
		symaux->vtype!=sym->vtype || symaux->nelements!=sym->nelements){
			printf("Line %d, col %d: Conflicting types (got ",sym->line,sym->column);
			printConflict(sym);
			printf(", expected ");
			printConflict(symaux);
			printf(")\n");
			return 0;
		}
	}
	
	return 1;
	//TODO
}


void printConflict(symptr sym){
	char* vtype;
	int i,j;
	symptr auxsym;
	
	switch(sym->vtype){
	
		case(Int):
			vtype="int";
			break;
		case(Char):
			vtype="char";
			break;
		case(Void):
			vtype="void";
			break;
		default:
			printf("ERROR IN FUNCTION PRINTSYMBOL.");
			break;
	}

	//variables
	if(sym->type==Var){
		printf("%s",vtype);
		
		for(i=0;i<sym->npointer;i++)
			printf("*");
		

	}
	//arrays
	else if(sym->type==Array){
		printf("%s",vtype);
		
		for(i=0;i<sym->npointer;i++)
			printf("*");
		
		printf("[%d]",sym->nelements);
		
	}
	else if(sym->type==Func){
		printf("%s",vtype);
		
		for(i=0;i<sym->npointer;i++)
			printf("*");
		
		printf("(");
		
		//empty parameter list
		if(sym->params->nelements==0){
			printf("void)");
			return;
		}
		
		auxsym=sym->params->first;
		
		//first parameter
		printVtype(auxsym->vtype);
		for(j=0;j<auxsym->npointer;j++)
			printf("*");
		
		//remaining parameters
		for(i=1;i<sym->params->nelements;i++){
				auxsym=auxsym->next;
				printf(",");
				printVtype(auxsym->vtype);
				for(j=0;j<auxsym->npointer;j++)
					printf("*");
		}
		
		printf(")");
	}		
}

//returns symbol table with the given id, or NULL
stptr searchst(char* id){
	stptr aux;
	
	aux=globalstlist->first;
	
	while(aux){
		if(aux->id){
			if(strcmp(aux->id,id)==0)
				return aux;
		}
		aux=aux->next;
	}
	
	return NULL;
}

int checkFunctionDecl(symptr sym){
	symptr aux;

	
	if(repetedParams(sym)){
		return 0;
	}
	
	aux=sym->params->first;
	
	while(aux){
		if(aux->npointer==0 && aux->vtype==Void){
			printf("Line %d, col %d: Invalid use of void type in declaration\n",aux->linetype,aux->columntype);
		}
		aux=aux->next;
	}
	
	if(sym->isdef==0){
		aux=searchSymbol(globalst,sym->id);
		if(aux!=NULL){
			
			if(aux->type==Func){
				
				if(compareFuncDecls(aux,sym)){
					return 1;
				}
			}
			
			printf("Line %d, col %d: Conflicting types (got ",sym->line,sym->column);
			printConflict(sym);
			printf(", expected ");
			printConflict(aux);
			printf(")\n");
			return 0;
		}
	
	
	}
	else{
		
		aux=searchSymbol(globalst,sym->id);
		if(!canFunctionBeDefined(sym)){
	
			printf("Line %d, col %d: Conflicting types (got ",sym->line,sym->column);
			printConflict(sym);
			printf(", expected ");
			printConflict(aux);
			printf(")\n");
			return 0;
		}
	}
	
	return 1;
	//TODO
}

//returns the symbol of a called function, or NULL if function doesn't exist or can't be applied to the given parameters
symptr checkCall(nodeptr np){
	
	symptr aux1,func;
	nodeptr aux3;
	char* id;
	
	id=np->children->first->lit;
	
	//search for function
	aux1=globalst->symbols->first;
	while(aux1!=NULL){
		if(strcmp(aux1->id,id)==0){
			//not a function
			if(aux1->type!=Func)
				return NULL;
			else
				break;
		}
		aux1=aux1->next;
	}
	func=aux1;
	
	if(func==NULL)
		return NULL;
	
	//wrong number of parameters
	if(np->children->nelements-1!=aux1->params->nelements)
		return NULL;
	
	aux1=func->params->first;
	aux3=np->children->first->next;
	while(aux3){
		
		//wrong parameter type
		if(strcmp(vartypeToStr(aux1->vtype),aux3->annotation)!=0){
			if(aux1->npointer!=0 || aux1->vtype==Array || aux3->npointer!=0 || aux3->isarray)
				if(!((strcmp(vartypeToStr(aux1->vtype),"char")==0 && strcmp(aux3->annotation,"int")==0)
				||(strcmp(vartypeToStr(aux1->vtype),"int")==0 && strcmp(aux3->annotation,"char")==0)))
					return NULL;	
		}
		if(!((aux1->npointer==aux3->npointer && aux3->isarray==0) || ((aux3->isarray && aux3->npointer+1 == aux1->npointer)))){
			return NULL;
		}
		
		aux1=aux1->next;
		aux3=aux3->next;
	}
	
	return func;
}

#define NEXPRESSIONS 26
char expressions[][15]={
	{"Or"}, {"And"}, {"Eq"}, {"Ne"}, {"Lt"},
	{"Gt"},{"Le"}, {"Ge"}, {"Add"},{"Sub"}, {"Mul"},
	{"Div"},{"Mod"}, {"Not"},{"Minus"}, {"Plus"}, {"Addr"}, {"Deref"}, {"Store"},
	{"Call"}, {"Comma"}, {"ChrLit"}, {"Id"}, {"IntLit"}, {"Pointer"}, {"StrLit"}
};

//says if a given id corresponds to the name of an expression.
//returns the expression if yes, or NULL if not
char* isExpression(char *id){
	int i;
	
	for(i=0;i<NEXPRESSIONS;i++){
		if(strcmp(id,expressions[i])==0)
			return expressions[i];
	}
	
	return NULL;
}

//returns size of string, taking into account escape characters.
int strlenescape(char* s){
	int i;
	int res;
	int aux;
	
	i=0;
	res=0;
	while(s[i]!='\0'){
		if(s[i]!='\\'){
			i++;
			res++;
			continue;
		}
		else{
			
			aux=0;
			i++;
			
			if(s[i]>'7' || s[i]<'0'){
				res++;
				i++;
				continue;
			}
			
			while(aux<3){
				if(s[i]>'7' || s[i]<'0'){
					break;
				}
				else{
					i++;
					aux++;
				}
			}
			res++;
		}
	}
	return res;
}

//annotates a given statements expressions, as well as the ones from it's children.
void annotateStatement(nodeptr np,stptr func){
	nodeptr aux,first,second;
	int haspointers=0;
	int hasundefs=0;
	symptr symaux;
	
	if(np==NULL)
		return;
	
	if(strcmp(np->type,"Null")==0){
		return;
	}
	
	//literals
	if(strcmp(np->type,"IntLit")==0){
		annotate(np,"int",0,0,0);
		return;
	}
	if(strcmp(np->type,"ChrLit")==0){
		annotate(np,"int",0,0,0);
		return;
	}
	if(strcmp(np->type,"StrLit")==0){
		//TODO o tamanho pode ter que ser incrementado em 1
		annotate(np,"char",0,1,strlenescape(np->lit)-1);
		return;
	}
	
	//IDs
	if(strcmp(np->type,"Id")==0){
		//TODO erro de o simbolo nao existir
		
		symptr sym=searchSymbol(func,np->lit);
		
		if(!sym){
			sym=searchSymbol(globalst,np->lit);
		}
		
		//TODO erro de o simbolo nao existir
		if(sym){
			if(sym->type==Array)
				annotate(np,vartypeToStr(sym->vtype),sym->npointer,1,sym->nelements);
			else if(sym->type==Var){
				np->isattributable=1;
				annotate(np,vartypeToStr(sym->vtype),sym->npointer,0,0);
			}
			else{
				annotate(np,vartypeToStr(sym->vtype),sym->npointer,0,0);
				np->isfunc=1;
				np->func=sym;
			}
		}
		else
			annotate(np,"undef",0,0,0);
		return;
	}
	
	aux=np->children->first;
	while(aux){
		annotateStatement(aux,func);
		
		if(aux->isannotated){
			if(strcmp(aux->annotation,"undef")==0){
				hasundefs=1;
			}
		
			if(aux->npointer>0 || aux->isarray)
				haspointers++;
		}
		aux=aux->next;
	}
	
	//fors,ifs 
	if(strcmp(np->type,"If")==0){
		//TODO first child must be int
		
		return;
	}
	if(strcmp(np->type,"For")==0){
		//TODO first 3 childs must be int
		
		return;
	}
	
	if(!isExpression(np->type))
		return;
			
	if(strcmp(np->type,"And")==0 || strcmp(np->type,"Or")==0){
		annotate(np,"int",0,0,0);
		return;
	}
		
	if(strcmp("Not",np->type)==0){
		annotate(np,"int",0,0,0);
		return;
	}
	
	if(hasundefs){
		annotate(np,"undef",0,0,0);
	}
	
	//call(special case)
	if(strcmp(np->type,"Call")==0){
		symaux=checkCall(np);
		symaux=searchSymbol(globalst,np->children->first->lit);
		
		if(symaux){
			annotate(np,vartypeToStr(symaux->vtype),symaux->npointer,0,0);
		}
		else{
			annotate(np,"undef",0,0,0);
		}
		return;
		
	}
	
	//comma(special case)
	if(strcmp(np->type,"Comma")==0){
		//TODO
		first=np->children->first;
		second=first->next;
		if(strcmp(first->annotation,"undef")==0){
			
			//TODO
		}
		
		if(second->isarray==0)
			annotate(np,second->annotation,second->npointer,second->isarray,second->nelements);
		else
			annotate(np,second->annotation,second->npointer+1,0,0);
		
		return;
	}
	
	//deref(special case)
	if(strcmp(np->type,"Deref")==0){
		//TODO
		first=np->children->first;
		
		if(first->isarray){
			annotate(np,first->annotation,first->npointer,0,0);
			np->isattributable=1;
		}
		else if(first->npointer>0){
			annotate(np,first->annotation,first->npointer-1,0,0);
			np->isattributable=1;
		}
		else{
			annotate(np,"undef",0,0,0);
		}
		return;
	}
	
	//Addr(special case)
	if(strcmp(np->type,"Addr")==0){
		//TODO
		first=np->children->first;
		
		annotate(np,first->annotation,first->npointer+1,0,0);
		return;
	}
	
	//Not,eq,...(special case)
	if(strcmp(np->type,"Not")==0 || strcmp(np->type,"Eq")==0 || strcmp(np->type,"Ne")==0
	|| strcmp(np->type,"Le")==0 || strcmp(np->type,"Lt")==0 || strcmp(np->type,"Ge")==0 || strcmp(np->type,"Gt")==0){
		annotate(np,"int",0,0,0);
		return;
	}
	
	//Store(special case)
	if(strcmp(np->type,"Store")==0){
		//TODO
		first=np->children->first;
		second=first->next;
		
		/*
		if(!first->isattributable){
			annotate(np,"undef",first->npointer,0,0);
			
			printf("Line %d, col %d: Lvalue required\n",np->line,np->column);
			
			return;
		}
		
		if(strcmp(second->annotation,"undef")==0 || strcmp(first->annotation,"undef")==0){
			annotate(np,"undef",0,0,0);
			return;
		}
		
		if(!(first->npointer==second->npointer ||
		 ((second->isarray && first->isarray==0 && second->npointer==first->npointer-1)))){
			annotate(np,"undef",first->npointer,0,0);
			
			//TODO
			
			return;
		}*/
		
		annotate(np,first->annotation,first->npointer,0,0);
		return;
	}
	
	
	
	//pointers
	if(haspointers){
		
		first=np->children->first;
		second=first->next;
		
		if(!second){
			
			annotate(np,"int",0,0,0);
			return;
		}
		
		if(strcmp(np->type,"Add")==0){
				
			
			if(first->npointer==0 && first->isarray==0){
				if(second->isarray)
					annotate(np,second->annotation,second->npointer+1,0,0);
				else annotate(np,second->annotation,second->npointer,0,0);
				return;
			}	
			
			else if(second->npointer==0 && second->isarray==0){
				if(first->isarray)
					annotate(np,first->annotation,first->npointer+1,0,0);
				else annotate(np,first->annotation,first->npointer,0,0);
				return;
			}
			else annotate(np,"undef",0,0,0);
			
			return;
		}
		if(strcmp("Sub",np->type)==0){
			
			if(haspointers==2){
				annotate(np,"int",0,0,0);
				return;
			}
			
			
			
			if(second->npointer==0 && second->isarray==0){
				if(first->isarray)
					annotate(np,first->annotation,first->npointer+1,0,0);
				else
					annotate(np,first->annotation,first->npointer,0,0);
				return;
			}
			else if(first->npointer==0 && first->isarray==0){
				if(second->isarray)
					annotate(np,second->annotation,second->npointer+1,0,0);
				else
					annotate(np,second->annotation,second->npointer,0,0);
				return;
			}
			
			if((first->npointer == second->npointer && first->isarray == second->isarray) 
				|| (first->npointer-1 == second->npointer && first->isarray && second->isarray==0)
				|| (first->npointer == second->npointer-1 && first->isarray==0 && second->isarray)){
					
				annotate(np,"int",0,0,0);
				return;
			}
				
			return;
		}
		
		
		else annotate(np,"undef",0,0,0);
	
	}
	//expressions not containing pointers
	else{
		
		first=np->children->first;
		
		if(first)
			second=first->next;
		else
			second=NULL;
		if(strcmp(first->annotation,"void")==0){
			annotate(np,"undef",0,0,0);
			return;
		}
			
		//binary operations
		if(second){
			if(strcmp(second->annotation,"void")==0){
				annotate(np,"undef",0,0,0);
				return;
			}
			
			if(strcmp(second->annotation,"char")==0 || strcmp(second->annotation,"int")==0){
				annotate(np,"int",0,0,0);
				return;
			}
			
			if(strcmp(first->annotation,"int")==0 || strcmp(first->annotation,"char")==0){
				annotate(np,"int",0,0,0);
				return;
			}
			
		}
		//unary operations
		else{
			if(strcmp(first->annotation,"int")==0 || strcmp(first->annotation,"char")==0){
				annotate(np,"int",0,0,0);
				return;
			}
		}
		
		annotate(np,"char",0,0,0);
		return;
		
	}
}

//returns the symbol table of a given function def tree node.
stptr getSymbolTable(nodeptr np){
	int i,tam;
	nodeptr aux,aux2;
	stptr res;
	symptr symaux;
	res=(stptr)malloc(sizeof(symtable));
	res->isfake=0;
	
	aux=np->children->first;
	
	res->returntype=getVType(aux->type);
	res->npointer=0;
	
	aux=aux->next;
	while(strcmp(aux->type,"Id")!=0){
		aux=aux->next;
		res->npointer++;
	}
	
	res->id=(char*)malloc(strlen(aux->lit)+1);
	res->id[0]='\0';
	strcpy(res->id,aux->lit);
	
	
	while(strcmp(aux->type,"ParamList")!=0){
		aux=aux->next;
	}
	
	tam=aux->children->nelements;
	aux2=aux;
	aux=aux->children->first;
	res->symbols=createSymList();
	
	//if parameter is just void, params will be an empty list
	if(!(tam==1 && aux->children->nelements==1 && strcmp(aux->children->first->type,"Void")==0)){
		for(i=0;i<tam;i++){
			addSymbol(res->symbols,createVarSymbol(aux,1));
			aux=aux->next;
		}
	}
	
	//we need to go back up the tree
	aux=aux2;
	
	while(strcmp(aux->type,"FuncBody")!=0){
		aux=aux->next;
	}
	
	tam=aux->children->nelements;
	
	//empty function body
	if(tam==0)
		return res;
	
	aux=aux->children->first;
	
	
	while(strcmp(aux->type,"Declaration")==0 || strcmp(aux->type,"ArrayDeclaration")==0){
		if(strcmp(aux->type,"Declaration")==0){
			symaux=createVarSymbol(aux,0);
			
			checkVarDecl(symaux,res);
			
			if(!searchSymbol(res,symaux->id))
				addSymbol(res->symbols,symaux);
		}
		else{
			symaux=createArraySymbol(aux);
			
			checkArrayDecl(symaux,res);
			
			if(!searchSymbol(res,symaux->id))
				addSymbol(res->symbols,symaux);
		}
		aux=aux->next;
		
		if(!aux)
			break;
	}
	
	//annotate tree
	while(aux){
		annotateStatement(aux,res);
		aux=aux->next;
	}
	
	
	return res;
}

//creates a fake symbol table to serve as placeholder for a function declaration
stptr newFakeST(nodeptr np){
	int i,tam;
	nodeptr aux,aux2;
	stptr res;
	res=(stptr)malloc(sizeof(symtable));
	res->isfake=1;
	
	aux=np->children->first;
	
	
	res->returntype=getVType(aux->type);
	res->npointer=0;
	
	aux=aux->next;
	while(strcmp(aux->type,"Id")!=0){
		aux=aux->next;
		res->npointer++;
	}
	
	res->id=(char*)malloc(strlen(aux->lit)+1);
	res->id[0]='\0';
	strcpy(res->id,aux->lit);
	
	
	while(strcmp(aux->type,"ParamList")!=0){
		aux=aux->next;
	}
	
	tam=aux->children->nelements;
	aux2=aux;
	aux=aux->children->first;
	res->symbols=createSymList();
	
	//if parameter is just void, params will be an empty list
	if(!(tam==1 && aux->children->nelements==1 && strcmp(aux->children->first->type,"Void")==0)){
			for(i=0;i<tam;i++){
				addSymbol(res->symbols,createVarSymbol(aux,1));
				aux=aux->next;
			}
	}
	
	return res;
}


//add pre defined functions to global scope
void initpredefs(stlptr stl){
	symptr aux;
	stptr auxst;
	
	//int atoi(char*)
	aux=(symptr)malloc(sizeof(symbol));	
	aux->id=newString("atoi");
	aux->vtype=Int;
	aux->type=Func;
	aux->npointer=0;
	aux->params=createSymList();
	addSymbol(aux->params,newParam(Char,1));
	addSymbolToST(globalst,aux);
	
	auxst=(stptr)malloc(sizeof(symtable));	
	auxst->isfake=1;
	auxst->id=newString("atoi");
	auxst->returntype=Int;
	auxst->npointer=0;
	auxst->symbols=createSymList();
	addSymbol(auxst->symbols,newParam(Char,1));
	addST(stl,auxst);
	
	
	aux=(symptr)malloc(sizeof(symbol));	
	aux->id=newString("itoa");
	aux->npointer=1;
	aux->vtype=Char;
	aux->type=Func;
	aux->params=createSymList();
	addSymbol(aux->params,newParam(Int,0));
	addSymbol(aux->params,newParam(Char,1));
	addSymbolToST(globalst,aux);
	
	auxst=(stptr)malloc(sizeof(symtable));	
	auxst->isfake=1;
	auxst->id=newString("itoa");
	auxst->returntype=Char;
	auxst->npointer=1;
	auxst->symbols=createSymList();
	addSymbol(auxst->symbols,newParam(Int,0));
	addSymbol(auxst->symbols,newParam(Char,1));
	addST(stl,auxst);
	
	aux=(symptr)malloc(sizeof(symbol));	
	aux->id=newString("puts");
	aux->type=Func;
	aux->vtype=Int;
	aux->npointer=0;
	aux->params=createSymList();
	addSymbol(aux->params,newParam(Char,1));
	addSymbolToST(globalst,aux);
	
	auxst=(stptr)malloc(sizeof(symtable));	
	auxst->isfake=1;
	auxst->id=newString("puts");
	auxst->returntype=Int;
	auxst->npointer=0;
	auxst->symbols=createSymList();
	addSymbol(auxst->symbols,newParam(Char,1));
	addST(stl,auxst);
}

//gets the symbol tables, both from the global scope and from each defined function.
stlptr getSymbolTables(int print){
	int i;
	
	stptr auxst;
	symptr symaux;
	nodeptr auxnp;
	globalstlist=newSTL();
	
	//the first st will be from the global scope, which has NULL id
	//and the pre defined functions
	auxst=(stptr)malloc(sizeof(symtable));
	auxst->id=NULL;
	auxst->symbols=createSymList();
	addST(globalstlist,auxst);
	auxst->isfake=0;
	globalst=auxst;
	
	//initialize predefined functions
	initpredefs(globalstlist);
	
	auxnp=ast->children->first;
	
	for(i=0;i<ast->children->nelements;i++){
		if(strcmp(auxnp->type,"FuncDefinition")==0){
			symaux=createFuncSymbol(auxnp);
			symaux->isdef=1;
			checkFunctionDecl(symaux);

			auxst=getSymbolTable(auxnp);
			addST(globalstlist,auxst);
			addSymbolToST(globalst,symaux);
		}
		else if(strcmp(auxnp->type,"FuncDeclaration")==0){
				symaux=createFuncSymbol(auxnp);
				symaux->isdef=0;
				checkFunctionDecl(symaux);

				auxst=newFakeST(auxnp);
				
				addST(globalstlist,auxst);
				addSymbolToST(globalst,symaux);
		}
		else if(strcmp(auxnp->type,"Declaration")==0){
			symaux=createVarSymbol(auxnp,0);
			if(checkVarDecl(symaux,globalst))
				addSymbolToST(globalst,symaux);
		}
		else if(strcmp(auxnp->type,"ArrayDeclaration")==0){
			symaux=createArraySymbol(auxnp);
			if(checkArrayDecl(symaux,globalst))
				addSymbolToST(globalst,symaux);

		}
		
		auxnp=auxnp->next;
	}
	
	if(print){
		printSTList(globalstlist);
		printTree(ast,0);
	}
	return globalstlist;
}

#endif



