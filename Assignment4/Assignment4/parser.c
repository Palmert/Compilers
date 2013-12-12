#include "parser.h"



/**********************************************************************************************************
Purpose:				Begins parsing a platypus source file
Author:					Svillen Ranev
History/Versions:		12.05.13
Called functions:		mlwpar_next_token(),program(),match(),gen_incode()
Parameters:				Buffer* in_buf
Return value:			none
Algorithm:				Get the next token in the file, call program(), call match to ensure that the 
						current token is SEOF, output that the source file has been parsed.
**********************************************************************************************************/
void parser(Buffer* in_buf)
{
	sc_buf = in_buf;
    lookahead_token = mlwpar_next_token(sc_buf);
    program();
    match(SEOF_T, NO_ATTR);
    //gen_incode("PLATY: Source file parsed");
}
/**********************************************************************************************************
Purpose:				Check to see if the next token matches the one we are expecting. 
Author:					Chris Whitten
History/Versions:		12.05.13
Called functions:		syn_printe(),mlwpar_next_token(),syn_eh()
Parameters:				int pr_token_code,int pr_token_attribute
Return value:			none
Algorithm:				Check if the the the attribute of the token needs to be matched, then if we are
						looking for SEOF_T then return otherwise advance the token by calling mlwpar_next_token();
						If the next token is an ERR_T call syn_printe() to print out the error and advance the token 
						again before returning. 
**********************************************************************************************************/
void match(int pr_token_code,int pr_token_attribute)
{
	/* Check to make sure the token is the one we are looking for before advancing the token.   */
    if ( (pr_token_attribute == NO_ATTR && lookahead_token.code == pr_token_code) 
		||(pr_token_attribute==lookahead_token.attribute.get_int&&lookahead_token.code==pr_token_code))
    {
		/* If we are looking for SEOF_T there is no need to advance the token so return. */
        if(lookahead_token.code == SEOF_T)
        {
            return;
        }
		
		/* Advance the token because the match was successful. */
        lookahead_token = mlwpar_next_token(sc_buf);

		/* If we're found an error token call print and advance the token and error count. */
        if(lookahead_token.code == ERR_T)
        {
            syn_printe();
            lookahead_token = mlwpar_next_token(sc_buf);
            ++synerrno;
            return;
        }
        return;
    }
	/* Match was not found call the recovery function to handle it cleanly. */
    syn_eh(pr_token_code);
    return;
}
/*********************************************************************************************************
Production:		program
Grammar:		PLAYTPUS {<opt_statements>}
First:			{ KW_T(PLATYPUS) }
Author			Svillen Ranev
*********************************************************************************************************/
void program(void)
{
    match(KW_T,PLATYPUS);
    match(LBR_T,NO_ATTR);
    opt_statements();
    match(RBR_T,NO_ATTR);
    //gen_incode("PLATY: Program parsed");
}
/*********************************************************************************************************
Production:		opt_statements
Grammar:		<statements>|e
First:			{ SVID , AVID, KW_T(IF), KW_T(USING) , KW_T(INPUT), KW_T(OUTPUT), e }
Author			Thome Palmer
*********************************************************************************************************/
void opt_statements(void)
{
    switch(lookahead_token.code)
    {
    case AVID_T:
    case SVID_T:
        statements();
        break;
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
        //gen_incode("PLATY: Opt_statements parsed");
    }
}
/*********************************************************************************************************
Production:		statements
Grammar:		<statement><statements'>
First:			{ SVID , AVID, KW_T(IF), KW_T(USING) , KW_T(INPUT), KW_T(OUTPUT) }
Author			Thom Palmer
*********************************************************************************************************/
void statements(void)
{
    statement();
    statements_p();
}
/*********************************************************************************************************
Production:		statements_p
Grammar:		<statement><statements'>|e
First:			{ SVID , AVID, KW_T(IF), KW_T(USING) , KW_T(INPUT), KW_T(OUTPUT) }
Author			Thom Palmer
*********************************************************************************************************/
void statements_p(void)
{
    switch(lookahead_token.code)
    {
    case AVID_T:
    case SVID_T:
        statements();
        break;
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
/*********************************************************************************************************
Production:		statement
Grammar:		<assignment statement>
				|<selection statement>
				|<iteration statement>
				|<input statement>
				|<output statement>
First:			{ SVID , AVID, KW_T(IF), KW_T(USING) , KW_T(INPUT), KW_T(OUTPUT) }
Author			Thom Palmer
*********************************************************************************************************/
void statement(void)
{
    switch(lookahead_token.code)
    {
    case SVID_T:
    case AVID_T:
        assignment_statement();
        break;
    case KW_T:
        switch(lookahead_token.attribute.get_int)
        {
        case IF:
            selection_statement();
            break;
        case USING:
            iteration_statement();
            break;
        case INPUT:
            input_statement();
            break;
        case OUTPUT:
            output_statement();
            break;
        }
    }
}
/*********************************************************************************************************
Production:		assignment_statement
Grammar:		<assignment expression>;
First:			{ SVID , AVID }
Author			Thom Palmer
*********************************************************************************************************/
void assignment_statement(void)
{
    assignment_expression();
	if(op_stack!=NULL)
	{
		while(op_stack->prevstackItem)
		{
			tl_addt(pop(&op_stack));
		}
		tl_addt(pop(&op_stack));
	}
	tl_addt(lookahead_token);
    match(EOS_T,NO_ATTR);
	gen_incode(ASS_OP_T);
	
    //gen_incode("PLATY: Assignment statement parsed");

}
/*********************************************************************************************************
Production:		assignment_expression
Grammar:		AVID = <arithmetic expression>
				|SVID = <string expression>
First:			{ SVID , AVID }
Author			Christopher Whitten
*********************************************************************************************************/
void assignment_expression(void)
{
    switch(lookahead_token.code)
    {
    case SVID_T:
		tl_addt(lookahead_token);
        match(SVID_T,NO_ATTR);
		tl_addt(lookahead_token);
        match(ASS_OP_T, NO_ATTR);
        string_expression();
        //gen_incode("PLATY: Assignment expression (string) parsed");
        break;
    case AVID_T:
		tl_addt(lookahead_token);
        match(AVID_T,NO_ATTR);
		tl_addt(lookahead_token);
        match(ASS_OP_T, NO_ATTR);
        arithmetic_expression();
        //gen_incode("PLATY: Assignment expression (arithmetic) parsed");
        break;
    default:
        syn_printe();
    }
}
/*********************************************************************************************************
Production:		selection_statement
Grammar:		IF(<conditional expression>)  THEN <opt_statements>
				ELSE { opt_statements };
First:			{ KW_T(IF) }
Author			Christopher Whitten
*********************************************************************************************************/
void selection_statement(void)
{
	tl_addt(lookahead_token);
    match(KW_T, IF);
    match(LPR_T, NO_ATTR );
    conditional_expression();
    match(RPR_T,NO_ATTR);
	tl_addt(lookahead_token);
    match(KW_T, THEN);
    opt_statements();
	tl_addt(lookahead_token);
    match(KW_T,ELSE);
    match(LBR_T, NO_ATTR);
    opt_statements();
    match(RBR_T,NO_ATTR);
	tl_addt(lookahead_token);
    match(EOS_T,NO_ATTR);
    gen_incode(IF);
}
/*********************************************************************************************************
Production:		iteration_statement
Grammar:		USING(<assignment expression>,<conditional expression>,<assignment expression>)
				REPEAT{
					<opt_statements>
				};
First:			{ KW_T(USING) }
Author			Christopher Whitten
*********************************************************************************************************/
void iteration_statement(void)
{
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
    //gen_incode("PLATY: USING statement parsed");
}


/*********************************************************************************************************
Production:		input_statement
Grammar:		INPUT(<variable list>);
First:			{ KW_T(INPUT) }
Author			Thom Palmer
*********************************************************************************************************/
void input_statement(void)
{
    match(KW_T,INPUT);
    match(LPR_T,NO_ATTR);
    variable_list();
    match(RPR_T,NO_ATTR);
    match(EOS_T,NO_ATTR);
    gen_incode(INPUT);
}
/*********************************************************************************************************
Production:		variable_list
Grammar:		<variable identifier><variable list'>
First:			{ SVID , AVID }
Author			Thom Palmer
*********************************************************************************************************/
void variable_list(void)
{
    variable_identifier();
    variable_list_p();
    //gen_incode("PLATY: Variable list parsed");
}
/*********************************************************************************************************
Production:		variable_list_p
Grammar:		,<variable identifier><variable list'>|e
First:			{ COM_T,e }
Author			Thom Palmer
*********************************************************************************************************/
void variable_list_p(void)
{
    if(lookahead_token.code == COM_T)
    {
        match(COM_T,NO_ATTR);
        variable_identifier();
        variable_list_p();
    }
}
/*********************************************************************************************************
Production:		variable_identifier
Grammar:		<arithmetic variable identifier> | <string variable identifier>
First:			{ SVID , AVID }
Author			Thom Palmer
*********************************************************************************************************/
void variable_identifier(void)
{
    switch(lookahead_token.code)
    {
    case SVID_T:
		tl_addt(lookahead_token);
        match(SVID_T,NO_ATTR);
        break;
    case AVID_T:
		tl_addt(lookahead_token);
        match(AVID_T,NO_ATTR);
        break;
    default:
        syn_printe();
    }
}
/*********************************************************************************************************
Production:		output_statement
Grammar:		OUTPUT(<output list>);
First:			{ KW_T(OUTPUT) }
Author:			Thom Palmer
*********************************************************************************************************/
void output_statement(void)
{	
	tl_addt(lookahead_token);
    match(KW_T,OUTPUT);	
    match(LPR_T,NO_ATTR);
    output_list();	
    match(RPR_T,NO_ATTR);
	tl_addt(lookahead_token);
    match(EOS_T,NO_ATTR);
	if(tkn_list->currToken.code == KW_T && tkn_list->currToken.attribute.get_int == OUTPUT)
    gen_incode(OUTPUT);
}
/*********************************************************************************************************
Production:		output_list
Grammar:		<variable list>|STR_T | e
First:			{ SVID , AVID ,STR_T, e )
Author:			Thom Palmer
*********************************************************************************************************/
void output_list(void)
{
    switch(lookahead_token.code)
    {
    case SVID_T:
    case AVID_T:
        variable_list();
        break;
    case STR_T:
		tl_addt(lookahead_token);
        match(STR_T,NO_ATTR);      
        break;
    default:
        //gen_incode("PLATY: Output list (empty) parsed");
        break;
    }
}
/*********************************************************************************************************
Production:		arithmetic_expression
Grammar:		<unary arithmetic expression>
				|<additive arithmetic expression>
First:			{ ART_OP_T(MINUS), ART_OP_T(PLUS) ,AVID_T, FPL_T, INL_T, LPR_T }
Author:			Thom Palmer
*********************************************************************************************************/
void arithmetic_expression(void)
{
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
    //gen_incode("PLATY: Arithmetic expression parsed");
}
/*********************************************************************************************************
Production:		unary_arithmetic_expression
Grammar:		- <primary arithmetic expression>
				|+<primary arithmetic expression>
First:			{ ART_OP_T(MINUS), ART_OP_T(PLUS) }
Author:			Thom Palmer
*********************************************************************************************************/
void unary_arithmetic_expression(void)
{
    if(lookahead_token.code  == ART_OP_T)
    {
        switch(lookahead_token.attribute.get_int)
        {
        case MINUS:
			while(op_stack != NULL && op_stack->currToken.attribute.get_int != LPR_T 
				&& (op_stack->currToken.attribute.get_int == MINUS || op_stack->currToken.attribute.get_int == PLUS ))
			{
				tl_addt(pop(&op_stack));
			}
			op_stack = push(op_stack,lookahead_token);
            match(ART_OP_T,MINUS);
            break;
        case PLUS:
			while(op_stack != NULL && op_stack->currToken.attribute.get_int != LPR_T 
				&& (op_stack->currToken.attribute.get_int == MINUS || op_stack->currToken.attribute.get_int == PLUS ))
			{
				tl_addt(pop(&op_stack));
			}
			op_stack = push(op_stack,lookahead_token);
            match(ART_OP_T,PLUS);
            break;
        default:
            syn_printe();
            break;
        }
    }
    else
    {
        syn_printe();
    }
    primary_arithmetic_expression();
    //gen_incode("PLATY: Unary arithmetic expression parsed");
}
/*********************************************************************************************************
Production:		additive_arithmetic_expression
Grammar:		<multiplicative arithmetic expression><additive arithmetic expression'>
First:			{ AVID_T, FPL_T, INL_T, LPR_T }
Author:			Thom Palmer
*********************************************************************************************************/
void additive_arithmetic_expression(void)
{
    multiplicative_arithmetic_expression();
    additive_arithmetic_expression_p();
}
/*********************************************************************************************************
Production:		additive_arithmetic_expression_p
Grammar:		+<multiplicative arithmetic expression><additive arithmetic expression'>
				|-<multiplicative arithmetic expression><additive arithmetic expression'>
				|e
First:			{ ART_OP_T(PLUS), ART_OP_T(MINUS), e }
Author:			Thom Palmer
*********************************************************************************************************/
void additive_arithmetic_expression_p(void)
{
    if(lookahead_token.code == ART_OP_T)
    {
        switch(lookahead_token.attribute.get_int)
        {
        case MINUS:
			if(!op_stack || op_stack->currToken.code == LPR_T  )
			{
				op_stack = push(op_stack,lookahead_token);
			}
			else
			{			
			while(op_stack != NULL && op_stack->currToken.code != LPR_T )
			{
				tl_addt(pop(&op_stack));
			}
			op_stack = push(op_stack,lookahead_token);
			}
            match(ART_OP_T,MINUS);
            multiplicative_arithmetic_expression();
            additive_arithmetic_expression_p();
            //gen_incode("PLATY: Additive arithmetic expression parsed");
            break;
        case PLUS:
			if(!op_stack || op_stack->currToken.code == LPR_T  )
			{
				op_stack = push(op_stack,lookahead_token);
			}
			else
			{
			while(op_stack != NULL && op_stack->currToken.code != LPR_T )
			{
				tl_addt(pop(&op_stack));
			}
			op_stack = push(op_stack,lookahead_token);
			}
            match(ART_OP_T,PLUS);
            multiplicative_arithmetic_expression();
            additive_arithmetic_expression_p();
            //gen_incode("PLATY: Additive arithmetic expression parsed");
            break;
        }
    }
    /*empty*/
}
/*********************************************************************************************************
Production:		multiplicative_arithmetic_expression
Grammar:		<primary arithmetic expression><multiplicative arithmetic expression'>
First:			{ AVID_T, FPL_T, INL_T, LPR_T }
Author:			Thom Palmer
*********************************************************************************************************/
void multiplicative_arithmetic_expression(void)
{
    primary_arithmetic_expression();
    multiplicative_arithmetic_expression_p();
}
/*********************************************************************************************************
Production:		multiplicative_arithmetic_expression_p
Grammar:		* <primary arithmetic expression><multiplicative arithmetic expression'>
				|/ <primary arithmetic expression><multiplicative arithmetic expression'>
				|e
First:			{ ART_OP_T(MULT), ART_OP_T(DIV), e }
Author:			Thom Palmer
*********************************************************************************************************/
void multiplicative_arithmetic_expression_p(void)
{
    /*FIRST( multiplicative arithmetic expression' ) = { ART_OP_T(MULT), ART_OP_T(DIV), e }*/
    if(lookahead_token.code == ART_OP_T)
    {
        switch(lookahead_token.attribute.get_int)
        {
        case MULT:
			if(!op_stack || op_stack->currToken.code == LPR_T  )
			{
				op_stack = push(op_stack,lookahead_token);
			}
			else
			{
			while(op_stack != NULL && op_stack->currToken.code != LPR_T 
				&& (op_stack->currToken.attribute.get_int == MULT || op_stack->currToken.attribute.get_int == DIV ))
			{
				tl_addt(pop(&op_stack));
			}
			op_stack = push(op_stack,lookahead_token);
			}
            match(ART_OP_T,MULT);
            primary_arithmetic_expression();
            multiplicative_arithmetic_expression_p();
            //gen_incode("PLATY: Multiplicative arithmetic expression parsed");
            break;
        case DIV:
			if(!op_stack || op_stack->currToken.code == LPR_T  )
			{
				op_stack = push(op_stack,lookahead_token);
			}
			else
			{
			while(op_stack != NULL && op_stack->currToken.code != LPR_T 
				&& (op_stack->currToken.attribute.get_int == MULT || op_stack->currToken.attribute.get_int == DIV ))
			{
				tl_addt(pop(&op_stack));
			}
			op_stack = push(op_stack,lookahead_token);
			}
            match(ART_OP_T,DIV);
            primary_arithmetic_expression();
            multiplicative_arithmetic_expression_p();
           // gen_incode("PLATY: Multiplicative arithmetic expression parsed");
            break;
        }
    }
    /*empty*/
}
/*********************************************************************************************************
Production:		primary_arithmetic_expression
Grammar:		  AVID_T
				| FPL_T
				| INL_T
				| (<arithmetic expression>)
First:			{ AVID_T, FPL_T, INL_T, LPR_T }
Author:			Thom Palmer
*********************************************************************************************************/
void primary_arithmetic_expression(void)
{
    switch(lookahead_token.code)
    {
    case AVID_T:
		tl_addt(lookahead_token);
        match(AVID_T,NO_ATTR);
        break;
    case FPL_T:
		tl_addt(lookahead_token);
        match(FPL_T,NO_ATTR);
        break;
    case INL_T:
		tl_addt(lookahead_token);
        match(INL_T,NO_ATTR);
        break;
    case LPR_T:
		op_stack = push(op_stack,lookahead_token);
        match(LPR_T,NO_ATTR);
        arithmetic_expression();
		while(op_stack->currToken.code != LPR_T)
		{
			tl_addt(pop(&op_stack));
		}
		pop(&op_stack);
        match(RPR_T, NO_ATTR);
        break;
    default:
        syn_printe();
    }
    //gen_incode("PLATY: Primary arithmetic expression parsed");
}
/*********************************************************************************************************
Production:		string_expression
Grammar:		<primary string expression><string expression'>
First:			{ SVID_T, STR_T }
Author:			Chris Whitten
*********************************************************************************************************/
void string_expression(void)
{
    primary_string_expression();
    string_expression_p();
    //gen_incode("PLATY: String expression parsed");
}
/*********************************************************************************************************
Production:		string_expression_p
Grammar:		<> <primary string expression><string expression'>
				|e
First:			{ SCC_OP_T, e }
Author:			Chris Whitten
*********************************************************************************************************/
void string_expression_p(void)
{
    if(lookahead_token.code == SCC_OP_T)
    {
		//tl_addt(lookahead_token);
        match(SCC_OP_T, NO_ATTR);
        primary_string_expression();
        string_expression_p();
    }
}
/*********************************************************************************************************
Production:		primary_string_expression
Grammar:		 SVID_T
				|STR_T
First:			{ SVID_T, STR_T }
Author:			Chris Whitten
*********************************************************************************************************/
void primary_string_expression(void)
{
    switch(lookahead_token.code)
    {
    case SVID_T:
		tl_addt(lookahead_token);
        match(SVID_T, NO_ATTR);
        break;
    case STR_T:
		tl_addt(lookahead_token);
        match(STR_T, NO_ATTR);
        break;
    default:
        syn_printe();
        break;
    }
    //gen_incode("PLATY: Primary string expression parsed");
}
/*********************************************************************************************************
Production:		conditional_expression
Grammar:		<logical OR expression>
First:			{ AVID_T, FPL_T, INL_T , SVID_T, STR_T }
Author:			Chris Whitten
*********************************************************************************************************/
void conditional_expression(void)
{
    logical_or_expression();
	if(op_stack!=NULL)
	{
		while(op_stack->prevstackItem)
		{
			tl_addt(pop(&op_stack));
		}
		tl_addt(pop(&op_stack));
	}
    //gen_incode("PLATY: Conditional expression parsed");
}
/*********************************************************************************************************
Production:		logical_or_expression
Grammar:		<logical AND expression><logical OR expression'>
First:			{ AVID_T, FPL_T, INL_T , SVID_T, STR_T }
Author:			Chris Whitten
*********************************************************************************************************/
void logical_or_expression(void)
{
    logical_and_expression();
    logical_or_expression_p();
}
/*********************************************************************************************************
Production:		logical_or_expression_p
Grammar:		.OR. <logical AND expression><logical OR expression'>
				|e
First:			{ LOG_OP_T(OR), e }
Author:			Chris Whitten
*********************************************************************************************************/
void logical_or_expression_p(void)
{
    if(lookahead_token.code == LOG_OP_T && lookahead_token.attribute.get_int == OR)
    {
		if(!op_stack)
		{
			op_stack = push(op_stack,lookahead_token);
		}
		else
		{			
			while(op_stack != NULL && (op_stack->currToken.code == LOG_OP_T || op_stack->currToken.code == REL_OP_T ))
			{
				tl_addt(pop(&op_stack));
			}
			op_stack = push(op_stack,lookahead_token);
		}
        match(LOG_OP_T, OR);
        logical_and_expression();
        logical_or_expression_p();
        //gen_incode("PLATY: Logical OR expression parsed");
    }
}
/*********************************************************************************************************
Production:		logical_and_expression
Grammar:		<relational expression><logical AND expression'>
First:			{ AVID_T, FPL_T, INL_T , SVID_T, STR_T }
Author:			Chris Whitten
*********************************************************************************************************/
void logical_and_expression(void)
{
    relational_expression();
    logical_and_expression_p();
}
/*********************************************************************************************************
Production:		logical_and_expression_p
Grammar:		.AND. <relational expression><logical AND expression'>
				|e
First:			{ LOG_OP_T(AND), e }
Author:			Chris Whitten
*********************************************************************************************************/
void logical_and_expression_p(void)
{
    if(lookahead_token.code == LOG_OP_T && lookahead_token.attribute.get_int == AND)
    {
		if(!op_stack)
		{
			op_stack = push(op_stack,lookahead_token);
		}
		else
		{			
			while(op_stack != NULL && ((op_stack->currToken.code == LOG_OP_T && op_stack->currToken.attribute.get_int == AND) || op_stack->currToken.code == REL_OP_T ))
			{
				tl_addt(pop(&op_stack));
			}
			op_stack = push(op_stack,lookahead_token);
		}
        match(LOG_OP_T, AND);
        relational_expression();
        logical_and_expression_p();
        //gen_incode("PLATY: Logical AND expression parsed");
    }
}
/*********************************************************************************************************
Production:		relational_expression
Grammar:		<primary a_relational expression> <relational operator> <primary a_relational expression>
				|<primary s_relational expression> <relational operator> <primary s_relational expression>
First:			{ AVID_T, FPL_T, INL_T , SVID_T, STR_T }
Author:			Chris Whitten
*********************************************************************************************************/
void relational_expression(void)
{
    switch(lookahead_token.code)
    {
    case AVID_T:
    case FPL_T:
    case INL_T:
        primary_a_relational_expression();
        relational_operator();
        primary_a_relational_expression();
        break;
    case SVID_T:
    case STR_T:
        primary_s_relational_expression();
        relational_operator();
        primary_s_relational_expression();
        break;
    default:
        syn_printe();
        break;
    }
    //gen_incode("PLATY: Relational expression parsed");
}
/*********************************************************************************************************
Production:		primary_a_relational_expression
Grammar:		 AVID_T
				|FPL_T
				|INL_T
First:			{ AVID_T, FPL_T, INL_T }
Author:			Chris Whitten
*********************************************************************************************************/
void primary_a_relational_expression(void)
{
    switch (lookahead_token.code)
    {
    case AVID_T :
		tl_addt(lookahead_token);
        match(AVID_T, NO_ATTR);
        break;
    case FPL_T:
		tl_addt(lookahead_token);
        match(FPL_T, NO_ATTR);
        break;
    case INL_T:
		tl_addt(lookahead_token);
        match(INL_T, NO_ATTR);
        break;
    default:
        syn_printe();
        break;
    }
    //gen_incode("PLATY: Primary a_relational expression parsed");
}
/*********************************************************************************************************
Production:		primary_s_relational_expression
Grammar:		<primary string expression>
First:			{ SVID_T, STR_T }
Author:			Thom Palmer
*********************************************************************************************************/
void primary_s_relational_expression(void)
{
    primary_string_expression();
    //gen_incode("PLATY: Primary s_relational expression parsed");
}
/*********************************************************************************************************
Production:		relational_operator
Grammar:		> | < | == | !=
First:			{REL_OP_T(GT),REL_OP_T(LT),REL_OP_T(EQ),REL_OP_T(NE)}
Author:			Chris Whitten
*********************************************************************************************************/
void relational_operator(void)
{
    if(lookahead_token.code == REL_OP_T)
    {
        switch(lookahead_token.attribute.get_int)
        {
        case LT:
			if(!op_stack)
			{
				op_stack = push(op_stack,lookahead_token);
			}
			else
			{			
				while(op_stack != NULL && op_stack->currToken.code == REL_OP_T)
				{
					tl_addt(pop(&op_stack));
				}
				op_stack = push(op_stack,lookahead_token);
			}
            match(REL_OP_T, LT);
            break;
        case GT:
			if(!op_stack)
			{
				op_stack = push(op_stack,lookahead_token);
			}
			else
			{			
				while(op_stack != NULL && op_stack->currToken.code == REL_OP_T)
				{
					tl_addt(pop(&op_stack));
				}
				op_stack = push(op_stack,lookahead_token);
			}
            match(REL_OP_T, GT);
            break;
        case EQ:
			if(!op_stack)
			{
				op_stack = push(op_stack,lookahead_token);
			}
			else
			{			
				while(op_stack != NULL && op_stack->currToken.code == REL_OP_T)
				{
					tl_addt(pop(&op_stack));
				}
				op_stack = push(op_stack,lookahead_token);
			}
            match(REL_OP_T,EQ);
            break;
        case NE:
			if(!op_stack)
			{
				op_stack = push(op_stack,lookahead_token);
			}
			else
			{			
				while(op_stack != NULL && op_stack->currToken.code == REL_OP_T)
				{
					tl_addt(pop(&op_stack));
				}
				op_stack = push(op_stack,lookahead_token);
			}
            match(REL_OP_T,NE);
            break;
        }
    }
    else
    {
        syn_printe();
    }
}
/**********************************************************************************************************
Purpose:				Parser error printing function
Author:					Svillen Ranev
History/Versions:		12.05.13
Called functions:		printf(),b_get_chmemloc()
Parameters:				none
Return value:			none
Algorithm:				Switch on the current token code, print the appropriate error response
**********************************************************************************************************/
void syn_printe()
{
    Token t = lookahead_token;

    printf("PLATY: Syntax error:  Line:%3d\n",line);
    printf("*****  Token code:%3d Attribute: ", t.code);
    switch(t.code)
    {
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
/**********************************************************************************************************
Purpose:				Advances in the source file until the desired token is found
Author:					Thom Palmer
History/Versions:		12.05.13
Called functions:		syn_printe(),mlwpar_next_token(),syn_eh(),exit()
Parameters:				int sync_token_code
Return value:			none
Algorithm:				Call syn_printe(), increment synerrno, scan the source file for the desired token
						or until SEOF is found, if SEOF is found exit the program, otherwise if the
						desired token is found and it is not SEOF advance to the next token
**********************************************************************************************************/
void syn_eh(int sync_token_code)
{
    syn_printe();				/* Call syn_printe to print the current error */
    ++synerrno;					/* Increment the error number */

	/* Continue scanning the source file until sync_token_code is found */
	while(lookahead_token.code != sync_token_code)
	{
		/* Exit the program if SEOF_T is found before sync_token_code is found */
		if(lookahead_token.code == SEOF_T)
		{
			exit(synerrno);
		}
		lookahead_token = mlwpar_next_token(sc_buf);
	}
	/* Advance to the next token */
	if(sync_token_code != SEOF_T)
	{
		lookahead_token = mlwpar_next_token(sc_buf);
	}
}
/**********************************************************************************************************
Purpose:				Prints a string
Author:					Thom Palmer
History/Versions:		12.05.13
Called functions:		printf()
Parameters:				char* production
Return value:			none		
**********************************************************************************************************/
void gen_incode( int code )
{
	TL* tempTL = tkn_list;
	int nullNotFound = 1;
	int i;

	while(nullNotFound)
	{
		if(tempTL->nextTLI == NULL)
		{
			nullNotFound = 0 ;
		}
		switch(code)
		{
		case OUTPUT:
			tl_printtl(tempTL);
			if(tempTL->prevTLI == NULL)
			{
				tl_destroy();
				return;
			}
			while(tempTL->nextTLI && tempTL->currToken.code != EOS_T)
			{
				tempTL = tempTL->nextTLI;
			}
			tempTL = tempTL->nextTLI;
			code = tempTL->currToken.code;
			break;
		case INPUT:
			tl_inputtl(tempTL);
			if(tempTL->prevTLI == NULL)
			{
				tl_destroy();
				return;
			}
			while(tempTL->nextTLI && tempTL->currToken.code != EOS_T)
			{
				tempTL = tempTL->nextTLI;
			}
			tempTL = tempTL->nextTLI;
			code = tempTL->currToken.code;
			break;
		case IF:
			tempTL = tempTL->nextTLI;
			if(psfx_parse_relop(tempTL))
			{
				while(tempTL->nextTLI && tempTL);
			}
			else
			{

			}
	
			break;
		case USING:
			break;
		case ASS_OP_T:
			tempTL = tempTL->nextTLI;
			sem_analyze(tempTL);
			if(tempTL->prevTLI->prevTLI == NULL)
			{
				tl_destroy();
				return;
			}
			
			break;
		case REL_OP_T:
			
			break;
		default:
			break;
		}
		if(tempTL->nextTLI)
			tempTL = tempTL->nextTLI;
	}
    printf("\nToken code = [ %d ]\n",code);
}

void tl_createtl(void)
{
	
}
void tl_addt( Token new_token )
{
	TL* tempTL;
	if(tkn_list == NULL)
	{
		tkn_list = (TL*)malloc(sizeof(TL));
		if(tkn_list == NULL)
		{
			exit(25);
		}
		tkn_list->prevTLI = NULL;
		tkn_list->currToken = lookahead_token;
		tkn_list->nextTLI = NULL;
		return;
	}
	tempTL = tkn_list;

	while(tempTL->nextTLI)
	{	
		tempTL = tempTL->nextTLI;
	}
	
	tempTL->nextTLI = (TL *)malloc(sizeof(TL));
	tempTL->nextTLI->prevTLI = tempTL;	
	tempTL = tempTL->nextTLI;
	tempTL->currToken = new_token;	
	tempTL->nextTLI = NULL;	
}
void tl_printtl(TL* tempTL)
{

	while(tempTL->nextTLI && tempTL->currToken.code != EOS_T)
	{	
		switch (tempTL->currToken.code)
		{
		case SVID_T:
			printf("%s",b_get_chmemloc(str_LTBL, sym_table.pstvr[tempTL->currToken.attribute.vid_offset].i_value.str_offset));
			break;
		case STR_T:
			printf("%s",b_get_chmemloc(str_LTBL, tempTL->currToken.attribute.str_offset));
			break;
		case AVID_T:
			switch(st_get_type(sym_table,tempTL->currToken.attribute.vid_offset))
			{
			case 'F':
				printf("%f",sym_table.pstvr[tempTL->currToken.attribute.vid_offset].i_value.fpl_val);
				break;
			case'I':
				printf("%d", sym_table.pstvr[tempTL->currToken.attribute.vid_offset].i_value.int_val);
				break;
			}
			break;
		default:
			break;
		}
		tempTL = tempTL->nextTLI;
	}
	switch (tempTL->currToken.code)
	{
	case SVID_T:
		printf("%s",b_get_chmemloc(str_LTBL, sym_table.pstvr[tempTL->currToken.attribute.vid_offset].i_value.str_offset));
		break;

	case STR_T:
		printf("%s",b_get_chmemloc(str_LTBL, tempTL->currToken.attribute.str_offset));
		break;
	case AVID_T:
		switch(st_get_type(sym_table,tempTL->currToken.attribute.vid_offset))
		{
		case 'F':
			printf("%f",sym_table.pstvr[tempTL->currToken.attribute.vid_offset].i_value.fpl_val);
			break;
		case'I':
			printf("%d", sym_table.pstvr[tempTL->currToken.attribute.vid_offset].i_value.int_val);
			break;
		}
		break;
	default:
		break;
	}
}

void tl_destroy(void)
{
	TL* tempTL;
	tempTL = tkn_list;
	tkn_list = NULL;
	while(tempTL->nextTLI)
	{	
		if(tempTL->prevTLI!=NULL)
		{
			free(tempTL->prevTLI);
			tempTL->prevTLI = NULL;
		}
		tempTL = tempTL->nextTLI;
	}
	free(tempTL);
}

void sem_analyze(TL* tempTL)
{
	InitialValue tempResult;	
	Token newValue = psfx_parse(tempTL);
	lvalue = tempTL->prevTLI->currToken;

		switch(newValue.code)
		{
		case FPL_T: 
			tempResult.fpl_val = newValue.attribute.flt_value;
			st_update_type(sym_table, lvalue.attribute.vid_offset, 'F');
			break;
		case INL_T:
			tempResult.int_val = newValue.attribute.int_value;
			st_update_type(sym_table,lvalue.attribute.vid_offset, 'I');
			break;
		case STR_T:
			tempResult.str_offset = newValue.attribute.str_offset;		
			break;
		}		
		st_update_value(sym_table, lvalue.attribute.vid_offset, tempResult);
}

Token psfx_parse(TL* tempTL)
{
	Token resultToken;
	Token opA;
	Token opB;
	int nullFound = 0;
	int arrFlag = 0;

	while(!nullFound)
	{
		if(!tempTL->nextTLI || tempTL->currToken.code == EOS_T)
		{
			++nullFound;
		}
		switch(tempTL->currToken.code)
		{
		case FPL_T:
		case INL_T:
		case AVID_T:
			tkn_stack = push(tkn_stack,tempTL->currToken);
			resultToken.code = FPL_T;			
			break;
		case SVID_T:
			break;
		case ART_OP_T:
			opB = pop(&tkn_stack);
			if(tkn_stack)
			opA = pop(&tkn_stack);
			else{
				opA.attribute.flt_value = 0.0;
			}
			switch(tempTL->currToken.attribute.get_int)
			{
				++arrFlag;
			case MINUS:
				if(opA.code == AVID_T && opB.code == AVID_T)
				{
					
					resultToken.attribute.flt_value =sym_table.pstvr[opA.attribute.vid_offset].i_value.fpl_val - sym_table.pstvr[opB.attribute.vid_offset].i_value.fpl_val;
				}
				if(opA.code != AVID_T && opB.code == AVID_T)
				{
					resultToken.attribute.flt_value = opA.attribute.flt_value - sym_table.pstvr[opB.attribute.vid_offset].i_value.fpl_val;
				}
				if(opA.code != AVID_T && opB.code != AVID_T)
				{
					resultToken.attribute.flt_value = opA.attribute.flt_value - opB.attribute.flt_value;
				}
				break;
			case PLUS:
				if(opA.code == AVID_T && opB.code == AVID_T)
				{
					resultToken.attribute.flt_value =sym_table.pstvr[opA.attribute.vid_offset].i_value.fpl_val + sym_table.pstvr[opB.attribute.vid_offset].i_value.fpl_val;
				}
				if(opA.code != AVID_T && opB.code == AVID_T)
				{
					resultToken.attribute.flt_value = opA.attribute.flt_value + sym_table.pstvr[opB.attribute.vid_offset].i_value.fpl_val;
				}
				if(opA.code != AVID_T && opB.code != AVID_T)
				{
					resultToken.attribute.flt_value = opA.attribute.flt_value + opB.attribute.flt_value;
				}
				break;
			case MULT:
				if(opA.code == AVID_T && opB.code == AVID_T)
				{
					resultToken.attribute.flt_value =sym_table.pstvr[opA.attribute.vid_offset].i_value.fpl_val * sym_table.pstvr[opB.attribute.vid_offset].i_value.fpl_val;
				}
				if(opA.code != AVID_T && opB.code == AVID_T)
				{
					resultToken.attribute.flt_value = opA.attribute.flt_value * sym_table.pstvr[opB.attribute.vid_offset].i_value.fpl_val;
				}
				if(opA.code != AVID_T && opB.code != AVID_T)
				{
					resultToken.attribute.flt_value = opA.attribute.flt_value * opB.attribute.flt_value;
				}
				break;
			case DIV:
				if(opA.code == AVID_T && opB.code == AVID_T)
				{
					resultToken.attribute.flt_value =sym_table.pstvr[opA.attribute.vid_offset].i_value.fpl_val / sym_table.pstvr[opB.attribute.vid_offset].i_value.fpl_val;
				}
				if(opA.code != AVID_T && opB.code == AVID_T)
				{
					resultToken.attribute.flt_value = opA.attribute.flt_value / sym_table.pstvr[opB.attribute.vid_offset].i_value.fpl_val;
				}
				if(opA.code != AVID_T && opB.code != AVID_T)
				{
					resultToken.attribute.flt_value = opA.attribute.flt_value / opB.attribute.flt_value;
				}
				break;				
			}
			tkn_stack = push(tkn_stack,resultToken);
		}
		tempTL = tempTL->nextTLI;
	
	}
	if(arrFlag == 0)
	{
		resultToken = pop(&tkn_stack);
	}
	return resultToken;
}

void tl_inputtl(TL* tempTL)
{
	char input [80];
	int i = 0;
	InitialValue inputValue;
	int dataType = INL_T;
	while(tempTL->nextTLI)
	{	
		switch (tempTL->currToken.code)
		{
		case SVID_T:
			scanf("%s",input);
			inputValue.str_offset = b_getsize(str_LTBL);
			st_update_value(sym_table,tempTL->currToken.attribute.vid_offset,inputValue);
			i=0;
			while(i<strlen(input)+1)
			{
				b_addc(str_LTBL,input[i++]);
			}		
			break;
		case AVID_T:
			scanf("%s",input);
			for(i=0;i<strlen(input);i++)
			{
				if(input[i] == '.')
				{
					dataType = FPL_T;
				}
			}
			if(dataType == FPL_T)
			{
				st_update_type(sym_table,tempTL->currToken.attribute.vid_offset,'F');
				inputValue.fpl_val = atodbl(input);
				st_update_value(sym_table,tempTL->currToken.attribute.vid_offset,inputValue);
			}
			else
			{
				st_update_type(sym_table,tempTL->currToken.attribute.vid_offset,'I');
				inputValue.int_val = atoint(input);
				st_update_value(sym_table,tempTL->currToken.attribute.vid_offset,inputValue);
			}
			break;
		
		}
		tempTL = tempTL->nextTLI;

	}
	switch (tempTL->currToken.code)
	{
	case SVID_T:
		scanf("%s",input);	
		inputValue.str_offset = b_getsize(str_LTBL);
		st_update_value(sym_table,tempTL->currToken.attribute.vid_offset,inputValue);
		i=0;
		while(i<strlen(input)+1)
		{
			b_addc(str_LTBL,input[i++]);
		}				
		break;
	case AVID_T:
		scanf("%s",input);
		for(i=0;i<strlen(input)+1;i++)
		{
			if(input[i] == '.')
			{
				dataType = FPL_T;
			}
		}
		if(dataType == FPL_T)
		{
			st_update_type(sym_table,tempTL->currToken.attribute.vid_offset,'F');
			inputValue.fpl_val = atodbl(input);
			st_update_value(sym_table,tempTL->currToken.attribute.vid_offset,inputValue);
		}
		else
		{
			st_update_type(sym_table,tempTL->currToken.attribute.vid_offset,'I');
			inputValue.int_val = atoint(input);
			st_update_value(sym_table,tempTL->currToken.attribute.vid_offset,inputValue);
		}
		break;
	default:
		break;
	}
	
}


TS* push( TS* stack, Token currToken)
{
	TS* temp = NULL;
	if(stack == NULL)
	{
		stack = (TS*)malloc(sizeof(TS));
		if(stack != NULL)
		{
			stack->currToken = currToken;
			stack->prevstackItem = NULL;
		}
		return stack;
	}
	temp = stack;
	stack = (TS*)malloc(sizeof(TS));	
	if(stack != NULL)
	{
		stack->currToken = currToken;
		stack->prevstackItem = temp;
		return stack;
	}
}

Token pop( TS** stack )
{
	TS* temp;
	Token currToken;
	temp = (*stack)->prevstackItem;
	currToken = (*stack)->currToken;
	free(*stack);
	*stack = NULL;
	if(temp!=NULL)
	*stack = temp;
	return currToken;
}

int exec_cond_s(TL* tempTL)
{
	Token rel_exp[4];
	unsigned int i;
	TL *temp_tk_list = tempTL;
	int nullNotFound = 0;
	int result = 0;

	while(nullNotFound || temp_tk_list->currToken.code != KW_T)
	{
		if(!temp_tk_list->nextTLI)
		{
			++nullNotFound;
		}
		for(i = 0; i<4;i++)
		{
			rel_exp[i] = temp_tk_list->currToken;
			if(temp_tk_list->nextTLI)
			temp_tk_list = temp_tk_list->nextTLI;
		}
		switch(rel_exp[1].attribute.get_int)
		{
		case LT:
			if(rel_exp[0].attribute.get_int < rel_exp[2].attribute.get_int)
				result = 1;
			else
				result = 0;
			break;
		case GT:
			if(rel_exp[0].attribute.get_int > rel_exp[2].attribute.get_int)
				result = 1;
			else
				result = 0;
			break;
		case EQ:
			if(rel_exp[0].attribute.get_int == rel_exp[2].attribute.get_int)
				result = 1;
			else
				result = 0;
			break;
		case NE:
			if(rel_exp[0].attribute.get_int != rel_exp[2].attribute.get_int)
				result = 1;
			else
				result = 0;
			break;
		}
		if(rel_exp[3].code == LOG_OP_T)
			switch(rel_exp[3].attribute.get_int)
			{
			case AND:
				if(result == 0)
				{
					return result;
				}
				break;
			case OR:
				if(result == 1)
				{
					return result;
				}
				break;
			}
	}
	return result;
}

int psfx_parse_relop(TL* tempTL)
{
	Token resultToken;
	Token opA;
	Token opB;
	int nullFound = 0;
	int arrFlag = 0;

	while(!nullFound)
	{
		if(!tempTL->nextTLI || tempTL->currToken.code == ASS_OP_T || tempTL->currToken.code == KW_T)
		{
			++nullFound;
		}

		switch(tempTL->currToken.code)
		{
		case FPL_T:
		case INL_T:
		case AVID_T:
			tkn_stack = push(tkn_stack,tempTL->currToken);
			resultToken.code = INL_T;			
			break;
		case SVID_T:
			tkn_stack = push(tkn_stack,tempTL->currToken);
			break;
		case REL_OP_T:
			opB = pop(&tkn_stack);
			if(tkn_stack)
				opA = pop(&tkn_stack);
			else{
				opA.code = AVID_T;
				opA.attribute.int_value = 0;
			}
			switch(tempTL->currToken.attribute.get_int)
			{
				
			case EQ:
				++arrFlag;
				if(opA.code == AVID_T && opB.code == AVID_T)
				{
					resultToken.attribute.int_value = opA.attribute.int_value == opB.attribute.int_value;
				}
				if(opA.code != AVID_T && opB.code == AVID_T)
				{
					resultToken.attribute.int_value = opA.attribute.flt_value == sym_table.pstvr[opB.attribute.vid_offset].i_value.fpl_val;
				}
				if(opA.code != AVID_T && opB.code != AVID_T)
				{
					resultToken.attribute.int_value = opA.attribute.flt_value == opB.attribute.flt_value;
				}
				break;
			case NE:
				++arrFlag;
				if(opA.code == AVID_T && opB.code == AVID_T)
				{
					resultToken.attribute.int_value =sym_table.pstvr[opA.attribute.vid_offset].i_value.fpl_val != sym_table.pstvr[opB.attribute.vid_offset].i_value.fpl_val;
				}
				if(opA.code != AVID_T && opB.code == AVID_T)
				{
					resultToken.attribute.int_value = opA.attribute.flt_value != sym_table.pstvr[opB.attribute.vid_offset].i_value.fpl_val;
				}
				if(opA.code != AVID_T && opB.code != AVID_T)
				{
					resultToken.attribute.int_value = opA.attribute.flt_value != opB.attribute.flt_value;
				}
				break;
			case GT:
				++arrFlag;
				if(opA.code == AVID_T && opB.code == AVID_T)
				{
					resultToken.attribute.int_value =sym_table.pstvr[opA.attribute.vid_offset].i_value.fpl_val > sym_table.pstvr[opB.attribute.vid_offset].i_value.fpl_val;
				}
				if(opA.code != AVID_T && opB.code == AVID_T)
				{
					resultToken.attribute.int_value = opA.attribute.flt_value > sym_table.pstvr[opB.attribute.vid_offset].i_value.fpl_val;
				}
				if(opA.code != AVID_T && opB.code != AVID_T)
				{
					resultToken.attribute.int_value = opA.attribute.flt_value > opB.attribute.flt_value;
				}
				break;
			case LT:
				++arrFlag;
				if(opA.code == AVID_T && opB.code == AVID_T)
				{
					resultToken.attribute.int_value =sym_table.pstvr[opA.attribute.vid_offset].i_value.fpl_val < sym_table.pstvr[opB.attribute.vid_offset].i_value.fpl_val;
				}
				if(opA.code != AVID_T && opB.code == AVID_T)
				{
					resultToken.attribute.int_value = opA.attribute.flt_value < sym_table.pstvr[opB.attribute.vid_offset].i_value.fpl_val;
				}
				if(opA.code != AVID_T && opB.code != AVID_T)
				{
					resultToken.attribute.int_value = opA.attribute.flt_value < opB.attribute.flt_value;
				}
				break;				
			}	
			op_stack = push(op_stack,resultToken);
			break;
			case LOG_OP_T:
				opB = pop(&op_stack);
				opA = pop(&op_stack);
				if(tempTL->currToken.attribute.get_int == AND)
				{
					++arrFlag;
					resultToken.attribute.int_value = opA.attribute.int_value && opB.attribute.int_value;
				}
				else
				{
					++arrFlag;
					resultToken.attribute.int_value = opA.attribute.int_value || opB.attribute.int_value;
				}
				op_stack = push(op_stack,resultToken);
				break;
				
		}
		
		tempTL = tempTL->nextTLI;

	}
	if(arrFlag == 0)
	{
		resultToken = pop(&tkn_stack);
	}
	return resultToken.attribute.get_int;
}


