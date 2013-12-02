#include "parser.h"
extern Token mlwpar_next_token(Buffer * sc_buf);
extern Buffer* str_LTBL;
extern STD sym_table;
extern int line;

void parser(Buffer* in_buf)
{

    sc_buf = in_buf;
    lookahead_token = mlwpar_next_token(sc_buf);
    program();
    match(SEOF_T, NO_ATTR);
    gen_incode("PLATY: Source file parsed");

}

void match(int pr_token_code,int pr_token_attribute)
{
	if( ( pr_token_attribute == NO_ATTR && lookahead_token.code == pr_token_code) || ( pr_token_attribute == lookahead_token.attribute.get_int && lookahead_token.code == pr_token_code) )
	{
		if(lookahead_token.code == SEOF_T)
		{
			return;
		}

		lookahead_token = mlwpar_next_token(sc_buf);

		if(lookahead_token.code == ERR_T)
		{
			syn_printe();
			lookahead_token = mlwpar_next_token(sc_buf);
			++synerrno;
			return;
		}
		return;
	}
	syn_eh(pr_token_code);
	return;
}

void program(void)
{
	match(KW_T,PLATYPUS);
	match(LBR_T,NO_ATTR);
	opt_statements();
	match(RBR_T,NO_ATTR);
	gen_incode("PLATY: Program parsed");
}


void opt_statements(void)
{
	/* FIRST set: {AVID_T,SVID_T,KW_T(but not PLATYPUS,ELSE,THEN,REPEAT),e} */
	switch(lookahead_token.code)
	{
	case AVID_T:
	case SVID_T: statements(); break;
	case KW_T:
		/* Check for PLATYPUS,ELSE,THEN,REPEAT here and in statements_p() */
		if(lookahead_token.attribute.get_int != PLATYPUS
			&& lookahead_token.attribute.get_int != ELSE 
			&& lookahead_token.attribute.get_int != THEN
			&& lookahead_token.attribute.get_int != REPEAT)
		{
			statements();
			break;
		}
	default: /* empty string - optional statements*/
		gen_incode("PLATY: Opt_statements parsed");
	}
}


void statements(void)
{
	statement();statements_p();
}
void statements_p(void)
{
	/*FIRST( statements' ) = { SVID , AVID, KW_T(IF), KW_T(USING) , KW_T(INPUT), KW_T(OUTPUT) }*/
	switch(lookahead_token.code)
	{
	case AVID_T:
	case SVID_T: statements(); break;
	case KW_T:
		/* Check for PLATYPUS,ELSE,THEN,REPEAT here and in statements_p() */
		if(lookahead_token.attribute.get_int != PLATYPUS
			&& lookahead_token.attribute.get_int != ELSE 
			&& lookahead_token.attribute.get_int != THEN
			&& lookahead_token.attribute.get_int != REPEAT)
		{
			statements();
			break;
		}
	default: /* empty string - optional statements*/
		break;
	}

}
void statement(void)
{
	/*FIRST( statement ) = { SVID , AVID, KW_T(IF), KW_T(USING) , KW_T(INPUT), KW_T(OUTPUT) }*/
	switch(lookahead_token.code)
	{
	case SVID_T:
	case AVID_T: assignment_statement(); break;
	case KW_T:
		switch(lookahead_token.attribute.get_int)
		{
		case IF: selection_statement(); break;			
		case USING: iteration_statement(); break;
		case INPUT: input_statement(); break;
		case OUTPUT: output_statement(); break;
		}
	}
}
void assignment_statement(void)
{
	/*FIRST( assignment statement ) = { SVID , AVID }*/
	assignment_expression(); match(EOS_T,NO_ATTR);
	gen_incode("PLATY: Assignment statement parsed");
}
void assignment_expression(void)
{
	/*FIRST(assignment expression) = { SVID , AVID }*/
	switch(lookahead_token.code)
	{
	case SVID_T: match(SVID_T,NO_ATTR); match(ASS_OP_T, NO_ATTR); string_expression(); gen_incode("PLATY: Assignment expression (string) parsed");break;
	case AVID_T: match(AVID_T,NO_ATTR); match(ASS_OP_T, NO_ATTR); arithmetic_expression(); gen_incode("PLATY: Assignment expression (arithmetic) parsed"); break;
	}
}
	
	
void selection_statement(void)
{
	/*FIRST( selection statement) = { KW_T(IF) }*/
	match(KW_T, IF);
	match(LPR_T, NO_ATTR );
	conditional_expression();
	match(RPR_T,NO_ATTR);
	match(KW_T, THEN);
	opt_statements();
	match(KW_T,ELSE);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T,NO_ATTR);
	match(EOS_T,NO_ATTR);
	gen_incode("PLATY: IF statement parsed");
}
void iteration_statement(void)
{
	/*FIRST( iteration statement ) = { KW_T(USING) }*/
	match(KW_T,USING);
	match(LPR_T,NO_ATTR);
	assignment_expression();
	match(COM_T, NO_ATTR);
	conditional_expression();
	match(COM_T, NO_ATTR);
	assignment_expression();
	match(RPR_T,NO_ATTR);
	match(KW_T,REPEAT);
	match(LBR_T,NO_ATTR);
	opt_statements();
	match(RBR_T,NO_ATTR);
	match(EOS_T,NO_ATTR);
	gen_incode("PLATY: USING statement parsed");
}
void input_statement(void)
{
	match(KW_T,INPUT);
	match(LPR_T,NO_ATTR);
	variable_list();
	match(RPR_T,NO_ATTR);
	match(EOS_T,NO_ATTR);
	gen_incode("PLATY: INPUT statement parsed");
}
void variable_list(void)
{
	/*FIRST( variable list ) = { SVID , AVID }*/
	variable_identifier();
	variable_list_p();
	gen_incode("PLATY: Variable list parsed");
}
void variable_list_p(void)
{
	/*FIRST( variable list' ) = { COM_T,e } */
	if(lookahead_token.code == COM_T)
	{
		match(COM_T,NO_ATTR);
		variable_identifier();variable_list_p();		
	}	
	
}
void variable_identifier(void)
{
	/*FIRST(variable identifier) = { SVID , AVID }*/
	switch(lookahead_token.code)
	{
	case SVID_T:
		match(SVID_T,NO_ATTR);
		break;
	case AVID_T:
		match(AVID_T,NO_ATTR);
		break;
	}
}
void output_statement(void)
{
	/*FIRST( output statement ) = { KW_T(OUTPUT) }*/
	match(KW_T,OUTPUT);
	match(LPR_T,NO_ATTR);
	output_list();
	match(RPR_T,NO_ATTR);
	match(EOS_T,NO_ATTR);
	gen_incode("PLATY: OUTPUT statement parsed");
}
void output_list(void)
{
	/*FIRST( output list ) = { SVID , AVID ,STR_T, e )*/
	switch(lookahead_token.code)
	{
	case SVID_T:
	case AVID_T: variable_list(); break;
	case STR_T: match(STR_T,NO_ATTR); gen_incode("PLATY: Output list (string literal) parsed"); break;
	default:
		gen_incode("PLATY: Output list (empty) parsed");
		break;
	}
	
}
void arithmetic_expression(void)
{
	/*FIRST( arithmetic expression ) = { ART_OP_T(MINUS), ART_OP_T(PLUS) ,AVID_T, FPL_T, INL_T, LPR_T }*/
	switch(lookahead_token.code)
	{
	case AVID_T:
	case FPL_T:
	case INL_T:
	case LPR_T: 
		additive_arithmetic_expression(); 
		break;
	case ART_OP_T: 
		if(lookahead_token.attribute.get_int != MULT && lookahead_token.attribute.get_int != DIV)
		unary_arithmetic_expression();
		break;
		
	}
	gen_incode("PLATY: Arithmetic expression parsed");
}
void unary_arithmetic_expression(void)
{
	/*FIRST( unary arithmetic expression ) = { ART_OP_T(MINUS), ART_OP_T(PLUS) }*/
	if(lookahead_token.code  == ART_OP_T)
	{
		switch(lookahead_token.attribute.get_int)
		{
		case MINUS:
			match(ART_OP_T,MINUS);
			break;
		case PLUS:
			match(ART_OP_T,PLUS);
			break;
		default:
			syn_printe();
			break;
		}		
		primary_arithmetic_expression(); 
		gen_incode("PLATY: Unary arithmetic expression parsed");
	}	
}
void additive_arithmetic_expression(void)
{
	/*FIRST( additive arithmetic expression ) = { AVID_T, FPL_T, INL_T, LPR_T }*/
	multiplicative_arithmetic_expression();
	additive_arithmetic_expression_p(); 
	
}
void additive_arithmetic_expression_p(void)
{
	/*FIRST( additive arithmetic expression' ) = { ART_OP_T(PLUS), ART_OP_T(MINUS), e }*/
	if(lookahead_token.code == ART_OP_T)
	{
		switch(lookahead_token.attribute.get_int)
		{
		case MINUS:
			match(ART_OP_T,MINUS);
			multiplicative_arithmetic_expression();
			additive_arithmetic_expression_p();
			gen_incode("PLATY: Additive arithmetic expression parsed");
			break;
		case PLUS:
			match(ART_OP_T,PLUS);
			multiplicative_arithmetic_expression();
			additive_arithmetic_expression_p();
			gen_incode("PLATY: Additive arithmetic expression parsed");
			break;
		}	
	}
	/*empty*/
	/*empty*/
}
void multiplicative_arithmetic_expression(void)
{
	/*FIRST( multiplicative arithmetic expression ) = { AVID_T, FPL_T, INL_T, LPR_T }*/
	primary_arithmetic_expression();
	multiplicative_arithmetic_expression_p(); 
	
}
void multiplicative_arithmetic_expression_p(void)
{
	/*FIRST( multiplicative arithmetic expression' ) = { ART_OP_T(MULT), ART_OP_T(DIV), e }*/
	if(lookahead_token.code == ART_OP_T)
	{
		switch(lookahead_token.attribute.get_int)
		{
		case MULT:
			match(ART_OP_T,MULT);
			primary_arithmetic_expression();
			multiplicative_arithmetic_expression_p();
			gen_incode("PLATY: Multiplicative arithmetic expression parsed");
			break;
		case DIV:
			match(ART_OP_T,DIV);
			primary_arithmetic_expression();
			multiplicative_arithmetic_expression_p();
			gen_incode("PLATY: Multiplicative arithmetic expression parsed");
			break;
		}	
	}
	/*empty*/
}
void primary_arithmetic_expression(void)
{
	/*FIRST( primary arithmetic expression ) = { AVID_T, FPL_T, INL_T, LPR_T }*/
	switch(lookahead_token.code)
	{
	case AVID_T: 
		match(AVID_T,NO_ATTR);  
		break;
	case FPL_T: 
		match(FPL_T,NO_ATTR); 
		break;
	case INL_T: 
		match(INL_T,NO_ATTR);
		break;
	case LPR_T: 
		match(LPR_T,NO_ATTR); 
		arithmetic_expression();  
		match(RPR_T, NO_ATTR);
		break;
	default:
		syn_printe();
	}
	gen_incode("PLATY: Primary arithmetic expression parsed");
}
void string_expression(void)
{
	/*FIRST( string expression ) = { SVID_T, STR_T}*/
	primary_string_expression();
	string_expression_p();
	gen_incode("PLATY: String expression parsed");
}
void string_expression_p(void)
{
	/*FIRST( string expression' ) = { SCC_OP_T, e }*/
	if(lookahead_token.code == SCC_OP_T)
	{
		match(SCC_OP_T, NO_ATTR);
		primary_string_expression();
		string_expression_p();
	}
	
}
void primary_string_expression(void)
{
	/*FIRST( primary string expression ) = { SVID_T, STR_T }*/
	switch(lookahead_token.code)
	{
	case SVID_T:
		match(SVID_T, NO_ATTR);
		break;
	case STR_T:	
		match(STR_T, NO_ATTR);
		break;
	default:
		break;
	}
	gen_incode("PLATY: Primary string expression parsed");
}
void conditional_expression(void)
{
	logical_or_expression();
	/*FIRST( conditional expression ) = { AVID_T, FPL_T, INL_T , SVID_T, STR_T }*/
	gen_incode("PLATY: Conditional expression parsed");
}
void logical_or_expression(void)
{
	logical_and_expression();
	logical_or_expression_p();
	/*FIRST( logical OR expression ) = { AVID_T, FPL_T, INL_T , SVID_T, STR_T }*/
	
}
void logical_or_expression_p(void)
{
	/*FIRST( logical OR expression' ) = { LOG_OP_T(OR), e }*/
	if(lookahead_token.code == LOG_OP_T && lookahead_token.attribute.get_int == OR){
		match(LOG_OP_T, OR);logical_and_expression();logical_or_expression_p();
		gen_incode("PLATY: Logical OR expression parsed");
	}	
}
void logical_and_expression(void)
{
	/*FIRST( logical AND expression ) = { AVID_T, FPL_T, INL_T , SVID_T, STR_T }*/
	relational_expression();logical_and_expression_p();
	
}
void logical_and_expression_p(void)
{
	/*FIRST( logical AND expression' ) = { LOG_OP_T(AND), e }*/
	if(lookahead_token.code == LOG_OP_T && lookahead_token.attribute.get_int == AND)
	{
		match(LOG_OP_T, AND);relational_expression();logical_and_expression_p();
		gen_incode("PLATY: Logical AND expression parsed");
	}
	

}
void relational_expression(void)
{
	/*FIRST( relational expression ) = { AVID_T, FPL_T, INL_T , SVID_T, STR_T }*/
	switch(lookahead_token.code)
	{
	case AVID_T:
	case FPL_T:
	case INL_T:
		primary_a_relational_expression();relational_operator();primary_a_relational_expression();
		break;
	case SVID_T:
	case STR_T:
		primary_s_relational_expression();relational_operator(); primary_s_relational_expression();
		break;
	}
	gen_incode("PLATY: Relational expression parsed");

}
void primary_a_relational_expression(void)
{
	/*FIRST ( primary a_relational expression ) = { AVID_T, FPL_T, INL_T }*/
	switch (lookahead_token.code)
	{
	case AVID_T :
		match(AVID_T, NO_ATTR);
		break;
	case FPL_T:
		match(FPL_T, NO_ATTR);
		break;
	case INL_T:
		match(INL_T, NO_ATTR);
		break;	
	}
	gen_incode("PLATY: Primary a_relational expression parsed");
}

void primary_s_relational_expression(void)
{
	/*FIRST( primary s_relational expression ) =  { SVID_T, STR_T }*/
	primary_string_expression();
	gen_incode("PLATY: Primary s_relational expression parsed");	
}

void relational_operator(void)
{
	/*FIRST(relational operator)= {REL_OP_T(GT),REL_OP_T(LT),REL_OP_T(EQ),REL_OP_T(NE)}*/
	if(lookahead_token.code == REL_OP_T)
	{
		switch(lookahead_token.attribute.get_int)
		{
			case LT: match(REL_OP_T, LT);
				break;
			case GT: match(REL_OP_T, GT);
				break;
			case EQ: match(REL_OP_T,EQ);
				break;
			case NE: match(REL_OP_T,NE);
				break;
		}
	}
}

/* Parser error printing function, Assignment 4, F13
 */
void syn_printe(){
Token t = lookahead_token;

printf("PLATY: Syntax error:  Line:%3d\n",line);
printf("*****  Token code:%3d Attribute: ", t.code);
switch(t.code){
	case  ERR_T: /* ERR_T     0   Error token */
		printf("%s\n",t.attribute.err_lex);
	 break;
	case  SEOF_T: /*SEOF_T    1   Source end-of-file token */
		printf("NA\n" );
	 break;
	case  AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
	case  SVID_T :/* SVID_T    3  String Variable identifier token */
		printf("%s\n",sym_table.pstvr[t.attribute.get_int].plex);
	 break;
	case  FPL_T: /* FPL_T     4  Floating point literal token */
		printf("%5.1f\n",t.attribute.flt_value);
	 break;
	case INL_T: /* INL_T      5   Integer literal token */
	        printf("%d\n",t.attribute.get_int);
	 break;
	case STR_T:/* STR_T     6   String literal token */
	        printf("%s\n",b_get_chmemloc(str_LTBL,(short)t.attribute.get_int));
	break;
        
        case SCC_OP_T: /* 7   String concatenation operator token */
	        printf("NA\n" );
	break;
	
	case  ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
		printf("NA\n" );
	break;
	case  ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
		printf("%d\n",t.attribute.get_int);
	break;
	case  REL_OP_T: /*REL_OP_T  10   Relational operator token */ 
		printf("%d\n",t.attribute.get_int);
	break;
	case  LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
		printf("%d\n",t.attribute.get_int);
	break;
	
	case  LPR_T: /*LPR_T    12  Left parenthesis token */
		printf("NA\n" );
	break;
	case  RPR_T: /*RPR_T    13  Right parenthesis token */
	        printf("NA\n" );
	break;
	case LBR_T: /*    14   Left brace token */
	        printf("NA\n" );
	break;
	case RBR_T: /*    15  Right brace token */
	        printf("NA\n" );
	break;
		
	case KW_T: /*     16   Keyword token */
	        printf("%s\n",kw_table[t.attribute.get_int]);
	break;
	
	case COM_T: /* 17   Comma token */
	        printf("NA\n");
	break;
	case EOS_T: /*    18  End of statement *(semi - colon) */
	        printf("NA\n" );
	break; 		
	default:
	        printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
	}
}


void syn_eh( int sync_token_code )
{
	syn_printe();
	++synerrno;

	for(lookahead_token = mlwpar_next_token(sc_buf); lookahead_token.code != sync_token_code; lookahead_token = mlwpar_next_token((sc_buf)))
	{
		if(lookahead_token.code == SEOF_T)
		{
			exit(synerrno);
		}
	}
	if(sync_token_code != SEOF_T)
	{
		lookahead_token = mlwpar_next_token(sc_buf);
	}
	return;	
}


void gen_incode( char* str )
{
	printf("%s\n",str);
}

//void gen_imc( char* str )
//{
//
//}


