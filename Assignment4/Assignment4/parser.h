/*********************************************************************************************************
File name:		parser.h
Compiler:		MS Visual Studio 2010
Authors:		Thom Palmer - 040 713 234 and Chris Whitten - 040 611 350 
Course:			CST 8152 - Compilers, Lab Section: 401
Assignment:		Assignment 4 
Date:			December 5th, 2013
Professor:		Sv. Ranev
Purpose:		Declare all the variables and functions in which are to be used in parser.c
*********************************************************************************************************/
#include "buffer.h"
#include "token.h"
#include "stable.h"




#define NO_ATTR -1		/* Used to indicate if the token doesn't have an attribute associated with it. */
#define ELSE 0			/* Represents the index value in the kw_table for ELSE */
#define IF 1			/* Represents the index value in the kw_table for IF */
#define INPUT 2			/* Represents the index value in the kw_table for INPUT */
#define OUTPUT 3		/* Represents the index value in the kw_table for OUTPUT */
#define PLATYPUS 4		/* Represents the index value in the kw_table for PLATYPUS */
#define REPEAT 5		/* Represents the index value in the kw_table for REPEAT */
#define THEN 6			/* Represents the index value in the kw_table for THEN */	
#define USING 7			/* Represents the index value in the kw_table for USING */


static Token lookahead_token;
static Buffer* sc_buf;
int synerrno;
extern char* kw_table[8];
extern Token mlwpar_next_token(Buffer * sc_buf);
extern Buffer* str_LTBL;
extern STD sym_table;
extern int line;
extern double atodbl(char * lexeme);
extern int atoint(char* lexeme);

typedef struct TokenListItem
{
	struct TokenListItem *prevTLI;
	Token currToken;
	int usingDepth;
	int ifDepth;
	struct TokenListItem *nextTLI;
}TL;

typedef struct TokenStack
{
	Token currToken;
	struct TokenStack *prevstackItem;
}TS;


TL *tkn_list;
TS *tkn_stack;
TS *op_stack;
TS *postfix_stack;
Token lvalue;
int usingDepth = 0;
int ifDepth = 0;

void parser(Buffer* in_buf);
void match(int pr_token_code,int pr_token_attribute);
void syn_eh(int sync_token_code);
void syn_printe(void);
void gen_incode(int code); 
void program(void);
void opt_statements(void);
void statements(void);
void statements_p(void);
void statement(void);
void assignment_statement(void);
void assignment_expression(void);
void selection_statement(void);
void iteration_statement(void);
void input_statement(void);
void variable_list(void);
void variable_list_p(void);
void variable_identifier(void);
void output_statement(void);
void output_list(void);
void arithmetic_expression(void);
void unary_arithmetic_expression(void);
void additive_arithmetic_expression(void);
void additive_arithmetic_expression_p(void);
void multiplicative_arithmetic_expression(void);
void multiplicative_arithmetic_expression_p(void);
void primary_arithmetic_expression(void);
void string_expression(void);
void string_expression_p(void);
void primary_string_expression(void);
void conditional_expression(void);
void logical_or_expression(void);
void logical_or_expression_p(void);
void logical_and_expression(void);
void logical_and_expression_p(void);
void relational_expression(void);
void primary_a_relational_expression(void);
void primary_s_relational_expression(void);
void relational_operator(void);

void tl_addt(Token currentToken, int usingDepth, int ifDepth);
void tl_destroy(void);
void tl_printtl(TL* tempTL);
void tl_inputtl(TL* tempTL);

void sem_analyze(TL* tempTL);
TS* push(TS* stack, Token currToken);
Token pop(TS** stack);
void createStack(TS* stack);


Token psfx_parse(TL* tempTL);
int psfx_parse_relop(TL* tempTL);
int exec_cond_s(TL* tempTL);






