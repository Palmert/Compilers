#include "parser.h"
extern Token mlwpar_next_token(Buffer * sc_buf);
extern Buffer* str_LTBL;
extern STD sym_table;
extern int line;

void parser(Buffer* in_buf)
{
	sc_buf = in_buf;
	lookahead_token = mlwpar_next_token(sc_buf);
	program(); match(SEOF_T, NO_ATTR);
	gen_incode("PLATY: Source file parsed");
}

void match(int pr_token_code,int pr_token_attribute)
{
	if(lookahead_token.code == pr_token_code ){
	switch(lookahead_token.code)
	{
	case KW_T:
	case LOG_OP_T:
	case ART_OP_T:
	case REL_OP_T: if(lookahead_token.attribute.get_int == pr_token_attribute)
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
			
	default:
		
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
	}
	}
	
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
	/*FIRST( statements ) = { SVID , AVID, KW_T(IF), KW_T(USING) , KW_T(INPUT), KW_T(OUTPUT) }*/
}
void statements_p(void)
{
	/*FIRST( statements' ) = { SVID , AVID, KW_T(IF), KW_T(USING) , KW_T(INPUT), KW_T(OUTPUT) }*/
}
void statement(void)
{
	/*FIRST( statement ) = { SVID , AVID, KW_T(IF), KW_T(USING) , KW_T(INPUT), KW_T(OUTPUT) }*/
}
void assignment_statement(void)
{
	/*FIRST( assignment statement ) = { SVID , AVID }*/
}
void assignment_expression(void)
{
	/*FIRST(assignment expression) = { SVID , AVID }*/
}
void selection_statement(void)
{
	/*FIRST( selection statement) = { KW_T(IF) }*/
}
void iteration_statement(void)
{
	/*FIRST( iteration statement ) = { KW_T(USING) }*/
}
void input_statement(void)
{
	match(KW_T,INPUT);
	match(LPR_T,NO_ATTR);
	variable_list();
	match(RPR_T,NO_ATTR);
	match(EOS_T,NO_ATTR);
	gen_incode("PLATY: Input statement parsed");
}
void variable_list(void)
{
	/*FIRST( variable list ) = { SVID , AVID }*/
}
void variable_list_p(void)
{
	/*FIRST( variable list' ) = { COM_T,e } */
}
void variable_identifier(void)
{
	/*FIRST(variable identifier) = { SVID , AVID }*/
}
void output_statement(void)
{
	/*FIRST( output statement ) = { KW_T(OUTPUT) }*/
}
void output_list(void)
{
	/*FIRST( output list ) = { SVID , AVID ,STR_T, e )*/
}
void arithmetic_expression(void)
{
	/*FIRST( arithmetic expression ) = { ART_OP_T(MINUS), ART_OP_T(PLUS) ,AVID_T, FPL_T, INL_T, LPR_T }*/
}
void unary_arithmetic_expression(void)
{
	/*FIRST( unary arithmetic expression ) = { ART_OP_T(MINUS), ART_OP_T(PLUS) }*/
}
void additive_arithmetic_expression(void)
{
	/*FIRST( additive arithmetic expression ) = { AVID_T, FPL_T, INL_T, LPR_T }*/
}
void additive_arithmetic_expression_p(void)
{
	/*FIRST( additive arithmetic expression' ) = { ART_OP_T(PLUS), ART_OP_T(MINUS), e }*/
}
void multiplicative_arithmetic_expression(void)
{
	/*FIRST( multiplicative arithmetic expression ) = { AVID_T, FPL_T, INL_T, LPR_T }*/
}
void multiplicative_arithmetic_expression_p(void)
{
	/*FIRST( multiplicative arithmetic expression' ) = { ART_OP_T(MULT), ART_OP_T(DIV), e }*/
}
void primary_arithmetic_expression(void)
{
	/*FIRST( primary arithmetic expression ) = { AVID_T, FPL_T, INL_T, LPR_T }*/
}
void string_expression(void)
{
	/*FIRST( string expression ) = { SVID_T, STR_T}*/
}
void string_expression_p(void)
{
	/*FIRST( string expression' ) = { SCC_OP_T, e }*/
}
void primary_string_expression(void)
{
	/*FIRST( primary string expression ) = { SVID_T, STR_T }*/
}
void conditional_expression(void)
{
	/*FIRST( conditional expression ) = { AVID_T, FPL_T, INL_T , SVID_T, STR_T }*/
}
void logical_or_expression(void)
{
	/*FIRST( logical OR expression ) = { AVID_T, FPL_T, INL_T , SVID_T, STR_T }*/
}
void logical_or_expression_p(void)
{
	/*FIRST( logical OR expression' ) = { LOG_OP_T(OR), e }*/
}
void logical_and_expression(void)
{
	/*FIRST( logical AND expression ) = { AVID_T, FPL_T, INL_T , SVID_T, STR_T }*/
}
void logical_and_expression_p(void)
{
	/*FIRST( logical AND expression' ) = { LOG_OP_T(AND), e }*/
}
void relational_expression(void)
{
	/*FIRST( relational expression ) = { AVID_T, FPL_T, INL_T , SVID_T, STR_T }*/
}
void primary_a_relational_expression(void)
{
	/*FIRST ( primary a_relational expression ) = { AVID_T, FPL_T, INL_T }*/
}
void primary_s_relational_expression(void)
{
	/*FIRST( primary s_relational expression ) =  { SVID_T, STR_T }*/
}
void relational_operator(void)
{
	/*FIRST(relational operator)= {REL_OP_T(GT),REL_OP_T(LT),REL_OP_T(EQ),REL_OP_T(NE)}*/
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
	        printf("%s\n",kw_table [t.attribute.get_int]);
	break;
	
	case COM_T: /* 17   Comma token */
	        printf("NA\n");
	break;
	case EOS_T: /*    18  End of statement *(semi - colon) */
	        printf("NA\n" );
	break; 		
	default:
	        printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
    }/*end switch*/
}/* end syn_printe()*/


void syn_eh( int sync_token_code )
{
	syn_printe();
	++synerrno;

	for(lookahead_token = mlwpar_next_token(sc_buf); lookahead_token.code != sync_token_code; lookahead_token = mlwpar_next_token((sc_buf)));
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


