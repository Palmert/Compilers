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

static Token lookahead;
static Token lookahead_token;
static Buffer* sc_buf;
int synerrno;
extern char* kw_table;

void parser(Buffer* in_buf);
void match(int pr_token_code,int pr_token_attribute);
void syn_eh(int sync_token_code);
void syn_printe(void);
void gen_incode(char* str); //Unsure about parameter name. Says it should be a string

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




