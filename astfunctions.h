#ifndef ASTFUNCTIONS
#define ASTFUNCTIONS

nodeptr createNode(char* type,listptr children,char* lit);

typedef struct symbolstr* symptr;

#define NUMTYPES 42
/*minimum children of each node type*/
char minchildren[NUMTYPES][2][21]={
	{"Program","1"}, 
	{"Declaration","2"}, 
	{"ArrayDeclaration","3"},
	{"FuncDeclaration","3"},
	{"FuncDefinition","4"},
	{"ParamList","1"},
	{"FuncBody","0"},
	{"ParamDeclaration","1"},
	{"StatList","2"},
	{"If","3"},
	{"For","4"},
	{"Return","1"},
	{"Or","2"}, {"And","2"}, {"Eq","2"}, {"Ne","2"}, {"Lt","2"},
	{"Gt","2"},{"Le","2"}, {"Ge","2"}, {"Add","2"},{"Sub","2"}, {"Mul","2"},
	{"Div","2"},{"Mod","2"}, {"Not","1"},{"Minus","1"}, {"Plus","1"}, {"Addr","1"}, {"Deref","1"}, {"Store","2"},
	{"Call","1"}, {"Comma","2"},
	{"Char","0"}, {"ChrLit","0"}, {"Id","0"}, {"Int","0"}, {"IntLit","0"}, {"Pointer","0"}, {"StrLit","0"}, {"Void","0"},
	{"Null","0"}	
};

void printConflict(symptr sym);

/*get minimum children of a node type*/
int getMinChildren(char* type){
	int i;
	for(i=0;i<NUMTYPES;i++){
		if(strcmp(type,minchildren[i][0])==0)
			return atoi(minchildren[i][1]);
	}
	return -1;
}

/*cria uma nova string com a capacidade dada*/
char* createString(int size){
	char* res;
	res=(char*)malloc((size+1)*sizeof(char));
	res[0]='\0';
	
	return res;
}

typedef struct node_str{
	char *type;
	
	//linha e coluna do no(meta 3)
	int column,line;
	
	int columntype,linetype;

	//whether or not the node is annotated and what that annotation is
	int isannotated;
	
	//whether the node can be used as the left side of an attribution
	int isattributable;
	
	char annotation[10];
	
	/*caso seja um literal*/
	char *lit;
	
	/*filhos do no*/
	listptr children;
	
	int npointer,nelements,isarray,isfunc;
	symptr func;
	
	/*irmao seguinte do no*/
	nodeptr next;
	
} node;

typedef struct list_str{
	nodeptr first;
	
	int nelements;
}list;

/*cria uma nova lista de nos*/
listptr createList(){
	listptr res;
	
	res=(listptr)malloc(sizeof(list));
	res->nelements=0;
	res->first=NULL;
	
	return res;
}

/*adiciona um novo no a lista e retorna o*/
nodeptr addNode(listptr lp,nodeptr np){
	int i;
	nodeptr aux;
	
	if(np==NULL)
		np=createNode("Null",NULL,NULL);
	
	/*lista vazia*/
	if(lp->nelements==0){
		lp->nelements++;
		lp->first=np;
		lp->first->next=NULL;
		return np;
	}
	
	aux=lp->first;
	for(i=0;i<lp->nelements-1;i++){
		aux=aux->next;
	}
	
	lp->nelements++;
	aux->next=np;
	np->next=NULL;
	return np;
}

/*concatenates two lists.returns the first one with the nodes of the second at the end.*/
listptr concatLists(listptr l1,listptr l2){
	nodeptr aux;
	int i;
	
	if(l1->nelements==0){
		l1->first=l2->first;
		l1->nelements=l2->nelements;
		free(l2);
		return l1;
	}
	
	aux=l1->first;
	
	for(i=0;i<l1->nelements-1;i++)
		aux=aux->next;
	
	aux->next=l2->first;
	l1->nelements+=l2->nelements;
	free(l2);
	return l1;
}



/*fills a node with "Null" children until it has a correct number of children.*/
void fixNode(nodeptr np){
	int i,reqchildren;
	
	reqchildren=getMinChildren(np->type);
	
	for(i=np->children->nelements;i<reqchildren;i++){
		addNode(np->children,createNode("Null",NULL,NULL));
	}
	
}

/*creates new node with the given type and literal value.
 literal value should be passed as a reference to the function(or NULL if not applicable).*/
nodeptr createNode(char* type,listptr children,char* lit){
	nodeptr res;
	char* straux;
	
	res=(nodeptr)malloc(sizeof(node));
	res->isannotated=0;
	res->isattributable=0;
	res->isarray=0;
	res->isfunc=0;
	res->nelements=0;
	res->func=NULL;
	
	if(!children)
		children=createList();
	
	res->children=children;
	
	res->type=createString(strlen(type));
	strcpy(res->type,type);
	
	
	
	/*each node type may or may not have a literal value*/
	if(strcmp("StrLit",type)==0 || strcmp("Id",type)==0 || strcmp("ChrLit",type)==0 || strcmp("IntLit",type)==0){
		straux=lit;
		res->lit=createString(strlen(straux));
		strcpy(res->lit,straux);
	}
	
	fixNode(res);
	
	return res;
}

//sets line and column number on node struct
void setLineColumn(nodeptr np,int line,int column){
	np->line=line;
	np->column=column;
}

//add annotation to node.
void annotate(nodeptr np,char* type,int npointer, int isarray,int nelements){
	np->isannotated=1;
	strcpy(np->annotation,type);
	np->npointer=npointer;
	np->isarray=isarray;
	np->nelements=nelements;
}

void deleteNode(nodeptr np){
	free(np->type);
	free(np->lit);
	free(np);
}

void printTree(nodeptr,int);



/*creates a declaration list from a typespec and a list of declarators*/
listptr createDeclarationList(nodeptr typespec,listptr declarators){
	listptr lp,res=createList();
	int isarray;
	nodeptr aux,lastdecl,new;
	char* type=typespec->type;
	int i;

	
	aux=declarators->first;
	for(i=0;i<declarators->nelements;i++){
		
		/*Array Declarator*/
		if(strcmp(aux->type,"DeclaratorArray")==0)
			isarray=1;
		/*Normal Declarator*/
		else isarray=0;

		lp=createList();
		addNode(lp,createNode(type,NULL,NULL));
		lp=concatLists(lp,aux->children);

		if(!isarray){
			new=createNode("Declaration",lp,NULL);
			setLineColumn(new,aux->line,aux->column);
		}
		else{
			new=createNode("ArrayDeclaration",lp,NULL);
			setLineColumn(new,aux->line,aux->column);
		}
		
		//printf("decl:%d,%d\n",new->line,new->column);
		addNode(res,new);
		
		
		lastdecl=aux;
		aux=lastdecl->next;
		deleteNode(lastdecl);
	}
	
	free(declarators);
	deleteNode(typespec);
	return res;
}

/*print AST on screen*/
void printTree(tree t,int depth){
	int i;
	nodeptr aux;
	char* type=t->type;

	

	for(i=0;i<depth;i++)
		printf("..");
	
	
	printf("%s",type);
	/*each node type may or may not have a literal value*/
	if(strcmp("StrLit",type)==0 || strcmp("Id",type)==0 || strcmp("ChrLit",type)==0 || strcmp("IntLit",type)==0){
		printf("(%s)",t->lit);
	}
	
	//print annotations if they exist
	if(t->isannotated){
		
		if(t->isfunc){
			///////
			printf(" - ");
			printConflict(t->func);

		}
		else{
			printf(" - %s",t->annotation);
			
			if(strcmp(t->annotation,"undef")!=0){
				for(i=0;i<t->npointer;i++)
					printf("*");
					
				 if(t->isarray)
					printf("[%d]",t->nelements);
			}
		}
	}
	

	printf("\n");
	aux=t->children->first;
	
	while(aux){
		printTree(aux,depth+1);
		aux=aux->next;
	}	
}


/*free memory occupied by tree*/
void deleteTree(tree t){
	nodeptr aux;

	aux=t->children->first;
	while(aux){
		deleteTree(aux);
		aux=aux->next;
	}
	
	free(t->type);
	free(t->lit);
	free(t->children);
	free(t);
}

#endif
