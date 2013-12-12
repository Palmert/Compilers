
#include "buffer.h"
#include "token.h"
#include "stable.h"




#define NO_ATTR -1
#define ELSE 0
#define IF 1
#define INPUT 2
#define OUTPUT 3
#define PLATYPUS 4
#define REPEAT 5
#define THEN 6
#define USING 7


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

int stackIndex;

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

void tl_addt(Token);
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






