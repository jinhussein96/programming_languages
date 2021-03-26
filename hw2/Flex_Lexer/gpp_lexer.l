
%{

#include <stdio.h>
extern int yylex();
extern int yylineno;
extern char* yytext;
char *names[] ={NULL,"KW_AND", "KW_OR", "KW_ NOT", "KW_EQUAL", "KW_LESS", "KW_NIL", "KW_LIST", "KW_APPEND", 
"KW_CONCAT","KW_SET", "KW_DEFFUN", "KW_FOR", "KW_IF", "KW_EXIT",
				 "KW_LOAD", "KW_DISP", "KW_TRUE", "KW_FALSE","OP_PLUS", "OP_MINUS", "OP_DIV", 
				 "OP_MULT",  "OP_OP", "OP_CP", "OP_DBLMULT", "OP_OC"," OP_CC", "OP_COMMA"};



%}

%%  


"and"					return 1;
"or"					return 2;
"not"               	return 3;
"equal"					return 4;
"less"   				return 5; 
"nil" 					return 6;
"list"   				return 7; 
"append" 				return 8;
"concat" 				return 9;
"set" 					return 10;
"deffun" 				return 11;
"for" 					return 12;
"if"  					return 13; 
"exit"  				return 14;
"load"  				return 15;
"disp"  				return 16;
"true"  				return 17;
"false"  				return 18;
"+"                     return 19;
"-"                     return 20; 
"/"                     return 21;
"*"						return 22;
"("                     return 23;
")"                     return 24;
"**"                    return 25;
["]                    return 26;
","                     return 28;
^\n                     return 100;
;;.*\n     				return 70;

[a-zA-Z][a-zA-Z0-9]*    return 29;
-?[0]|[1-9][0-9]*         return 30;
[ \t\n]                ;
[0-9][0-9a-zA-Z]*         	return -1;




.                      return 90;

%%
int yywrap(){
 	return 1;
}

int main(int argc, char **argv){
	FILE* point ;
	
	extern FILE* yyout;
	yyout = fopen("parsed_cpp.txt","w");



	if(argc > 1){
		point = fopen(argv[1] , "r");
		yyin = point;
	
		if(point == NULL)
		{
			printf("cant open file\n");
			exit(0);
		}
	}
	int token ;
		token = yylex();
	
		while(token){
			if(token == 100 && point == NULL){
				yyterminate();
			}
			if(token <= 28 && token > 0){
				fprintf(yyout,"%s\n",names[token]);
			}
			else if(token == 90){
				fprintf(yyout ,"ERROR\n");
				
			}
			else if(token == 29)
				fprintf(yyout,"IDENTIFIER \n");
			else if(token == 30 )
				fprintf(yyout,"VALUE\n");
			else if(token == 70)
					fprintf(yyout,"COMMENT \n");
			else if(token == -1){
				fprintf(yyout,"ERROR %s \n",yytext);
				
			}
	
			token = yylex();

			
		
		}
		fclose(point);
		fclose(yyout);
		


}
