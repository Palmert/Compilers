/*********************************************************************************************************
File name:		scanner.c
Compiler:		MS Visual Studio 2110
Authors:		Thom Palmer - 040 713 234 and Chris Whitten - 040 611 350 
Course:			CST 8152 – Compilers, Lab Section: 401
Assignment:		Assignment 2 
Date:			Oct. 25th 2013
Professor:		Sv. Ranev
Version:		10.24.13
Purpose:		Functions implementing a Lexical Analyzer (Scanner) as required for CST8152, Assignment #2
Function list:	scanner_init(), mwlpar_next_token(), get_next_state(), char_class(), a_func02(),
				aa_func03(), aa_func05(), aa_func08(), aa_func11(), aafunc12(),  isKeyword()				
*********************************************************************************************************/

/* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
 * to suppress the warnings about using "unsafe" functions like fopen()
 * and standard sting library functions defined in string.h.
 * The define does not have any effect in Borland 5.02 projects.
 */
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

/*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "buffer.h"
#include "token.h"
#include "table.h"
#include "stable.h"

#define DEBUG  /* for conditional processing */
#undef  DEBUG

/* Global objects - variables */
/* This buffer is used as a repository for string literals.It is defined in platy_st.c */
extern Buffer * str_LTBL; /*String literal table */
int line; /* current line number of the source code */
extern int scerrnum;     /* defined in platy_st.c - run-time error number */
extern STD sym_table;

/* Local(file) global objects - variables */
static Buffer *lex_buf;/*pointer to temporary lexeme buffer*/

/* No other global variable declarations/definitiond are allowed */

/* scanner.c static(local) function  prototypes */ 
static int char_class(char c);					/* character class function */
static int get_next_state(int, char, int *);	/* state machine function */
static int iskeyword(char * kw_lexeme);			/* keywords lookup functuion */
static double atodbl(char * lexeme);			/* Converts string to double */
static long atool(char * lexeme);				/* Converts octal string to decimal value */
static int atoint(char * lexeme);				/* Converts integer literal string to int value */

int scanner_init(Buffer * sc_buf) {
  	if(b_isempty(sc_buf)) return EXIT_FAILURE;/*1*/
	b_set_getc_offset(sc_buf,0);/* in case the buffer has been read previously  */
	b_reset(str_LTBL);
	line = 1;
	return EXIT_SUCCESS;/*0*/
/*   scerrnum = 0;  *//*no need - global ANSI C */
}
/**********************************************************************************************************
Purpose:			Set the proper token depending on the lexeme read from the buffer. 
Author:				Thom Palmer and Chris Whitten
History/Versions:	10.20.13
Called functions:	b_setmark(), b_getc(), b_getmark(), SEOF(), WHTSPACE(), is_assop(), strncmp(),
					b_set_getc_offset(), b_retract(), b_get_getc_offset(),b_eob(), isalnum(), b_addc()
					get_next_state(), b_create(), b_pack(), b_destroy() 
Parameters:			Buffer * const pBD
Return value:		Token t
Algorithm:			Validate parameters, read char from buffer, check the char to see if its a legal character.
					depending on the char peak forward to see what the following char is in order to 
					determine if it's part of the lexeme or if it's time to return. once a valid lexeme is
					found set the token and return. if a valid lexeme is not found set the token to error 
					state and return. 
**********************************************************************************************************/
Token mlwpar_next_token(Buffer * sc_buf)
{
	Token t;			/* token to return after recognition */
	unsigned char c;	/* input symbol */
	int state = 0;		/* initial state of the FSM */
	unsigned int retrCtr;
	short lexstart;		/* start offset of a lexeme in the input buffer */
	short lexend;		/* end offset of a lexeme in the input buffer */
	int accept = NOAS;	/* type of state - initially not accepting */  
	unsigned int i=0;	/* Used throughout the function as iterator */
   
	/* Ensure the buffer is not null before trying to access it */

	if(sc_buf == NULL)
	{	   
		scerrnum = BUFFNULL;  
	     t_set_err_t(RUNTIMERR, t);
	}     
                
	while (1) /* Endless loop broken by token returns it will generate a warning */
	{ 
                
		/* Set mark before getting the next character. This prevents the need for unneccessary decrements. */
		b_setmark(sc_buf, b_get_getc_offset(sc_buf));
		lexstart = b_getmark(sc_buf);
	
		/* Get the next char from the buffer. */
		c = b_getc(sc_buf); 	
	
		/* Ensure SEOF has not been read before processing any further. */
		if(SEOF(c))
		{
			t.code = SEOF_T;
			return t;
		}

		/* If it's whitespace ignore and return to the start of the loop. */
		if(WHTSPC(c))
		{
			continue;
		}

		/* Drop into switch statement to determine what the character can potentially represent and handle it appropriatly. */
		switch(c)
		{
			/* If c is '=' the token can either be a relation operatory or an assignment operator,  so peak forward */
			case EQSIGN:
				{ 
					c = b_getc(sc_buf);
					/* If the next character is '=' then we have found a relational operator */
					if(c == EQSIGN) 
					{
						/* Set the code and attribute and return t */
						t.code = REL_OP_T;
						t.attribute.rel_op = EQ;
						return t;
					}
					/* Otherwise retract and return an assignment operator token */
					b_retract(sc_buf);
					t.code = ASS_OP_T;
					return t;
				} 
			/* If the token starts with ! it can either be a comment or the != relational operator, so peak forward and act appropriatly. */
			case EXCLAMTN:
				c = b_getc(sc_buf);
				/* If the next token is < then we have a comment therfore ignore everything in the line.*/
				if(c == LESSTHN)
				{
					do
					{
						c = b_getc(sc_buf);
						if(SEOF(c))
						{ 
							t.code = ERR_T;
							t.attribute.err_lex[0] = EXCLAMTN;
							t.attribute.err_lex[1] = LESSTHN;
							t.attribute.err_lex[2] = c;
							t.attribute.err_lex[3] = STRTERM;
							b_retract(sc_buf);
							return t;
						}
					}while ( c != NEWLINE && c != CARRTRN);

					++line;
					continue;
				}
				/* If the next token we have the NE relational operator. */
				if(c == EQSIGN)
				{
					t.code = REL_OP_T;
					t.attribute.rel_op = NE ;
					return t;
				}
				/* if the next char is neither = or < we have an error set ERR_T*/
				t.code = ERR_T;
				t.attribute.err_lex[0] = EXCLAMTN;
				t.attribute.err_lex[1] = c;
				t.attribute.err_lex[2] = STRTERM;
	   
				/* We assume the error was meant to be a token so ignore everything in the line. */
				do
				{
					c = b_getc(sc_buf);
					/* If SEOF or b_eob is found retract the buffer and return t */
					if(SEOF(c))
					{
						b_retract(sc_buf);
						return t;
					}
				}while ( c != NEWLINE);
				++line;
				return t;

			/*If we have a plus sign '+' set the token and it's attirbute then return. */
			case POS:
				t.code = ART_OP_T;
				t.attribute.arr_op = PLUS;
				return t;

			/*If we have a minus sign '-' set the token and it's attirbute then return. */
			case NEG:
				t.code = ART_OP_T;
				t.attribute.arr_op = MINUS;
				return t;

			/*If we have a a period '.' it could be a logical operator or an error. */
			case PERIOD:
				retrCtr = 1;
				i = 0;
				c = b_getc(sc_buf);
				
				/* Switch on the first character read after the period '.' */
				switch (c) 
				{
				/* If its an 'A' we might have .AND. */
				case'A' :
					/* Compare the string the string read from the buffer to the string literal .AND.  */
					++retrCtr;
					if(b_getc(sc_buf) == 'N')
					{
						++retrCtr;
						if(b_getc(sc_buf) == 'D')
						{
							++retrCtr;
							if(b_getc(sc_buf) == PERIOD)
							{
								t.code = LOG_OP_T;
								t.attribute.log_op = AND;
								return t;
							}
						}
					}
					break;
				case'O':
					/* Comapre the string the string read from the buffer to the string literal .OR.  */
					++retrCtr;
					if(b_getc(sc_buf) == 'R')
					{
						++retrCtr;
						if(b_getc(sc_buf) == PERIOD)
						{
							t.code = LOG_OP_T;
							t.attribute.log_op = OR;
							return t;
						}
					}

					break;
				}

				while(i<retrCtr)
				{
					b_retract(sc_buf);
					i++;
				}
				t.code = ERR_T;
				/* Add char which caused the error to the err_lex */
				t.attribute.err_lex[0] = PERIOD;
				t.attribute.err_lex[1] = STRTERM;
				return t;

			/* If we have an astrix sign '*' set the token and it's attirbute then return. */
			case ASTRX:
				t.code = ART_OP_T;
				t.attribute.arr_op = MULT;
				return t;

			/* If c is a forward slash '/' set the token and it's attirbute then return. */
			case FWDSLSH:
				t.code = ART_OP_T;
				t.attribute.arr_op = DIV;
				return t;

			/* If c is a left brace '{' set the token and it's attirbute then return. */
			case LBRACE:
				t.code = LBR_T;
				return t;

			/* If c is a right brace '}' set the token and it's attirbute then return. */
			case RBRACE:
				t.code = RBR_T;
				return t;

			/* If c is a left parenthesis '(' set the token and it's attirbute then return. */
			case LPRNTHS:
				t.code = LPR_T;
				return t;

			/* If c is a right parenthesis ')' set the token and it's attirbute then return. */
			case RPRNTHS:
				t.code = RPR_T;
				return t;

			/* If c is a less than symbol '<' check the next char. */
			case LESSTHN:
				c = b_getc(sc_buf);

				/* If the next char is the greater than symbol '>' set the proper token and return. */
				if(c == GRTRTHN){
					t.code = SCC_OP_T;
					return t;
				}

				/* If the next char is not recognized restract the buffer and set the token */
				t.code = REL_OP_T;
				t.attribute.rel_op = LT;
				b_retract(sc_buf);
				return t;

			/* If c is a greater than symbol '>' set the proper token and return. */
			case GRTRTHN:
				t.code = REL_OP_T;
				t.attribute.rel_op = GT;
				return t;

			/* If c is a new line '\n' increment the line number and return to the start of the loop. */
			case CARRTRN:
				NEWLINE_TEST
				continue;
				
			/* If c is a NEWLINE character increment the line number and continue */
			case NEWLINE:
				++line;
				continue;

			/* If c is a comma ',' set the proper token and return. */
			case COMMA:
				t.code = COM_T;  
				return t;

			/* If c is a semi colon ';' set the proper token and return. */
			case SEMICLN:
				t.code = EOS_T;
				return t;

			/* If c is a quotation mark '"' we have the start of a string, analyze the next chars until '"' or SEOF is hit. */
			case QUOTE:
				/* read all the chars in from the input buffer until a '"' */
				do
				{
					c = b_getc(sc_buf);
					lexend = b_get_getc_offset(sc_buf);
					/* If eob has be set or SEOF is read in from the buffer prior to closing the string */
					/* Break into the error token setup. */
					if( SEOF(c))
					{	
						/* Set the getc_offset to the start of the string */
						b_set_getc_offset(sc_buf,lexstart);
						/* Iterate over the buffer and copy the contents of the error string into the err_lex */
						for( i = 0; i < lexend-lexstart; i++) /* Comparison of unsigned int and short will generate warning. 
															     lexend-lexstart will always be positive no need to cast */
						{				
							c = b_getc(sc_buf);
							/* For the first 20 characters */
							if(i<=ERR_LEN)
							{
								/* Copy c into the current index of err_lex for first 16 characters */
								if(i<=16)
								t.attribute.err_lex[i] = c;
								/* Copy a decimal into the indexes between 17 and ERR_LEN */
								if(i>16 && i<ERR_LEN)
								{
									t.attribute.err_lex[i] = PERIOD;
								}
								/* Copy a string terminator into the last index of err_lex */
								if (i==ERR_LEN)
								{
									t.attribute.err_lex[i]= STRTERM;
								}

							}
					
						}
						t.code = ERR_T;							
						return t;
					}
					/* Increment the line number each time a line terminator is found */
					if(c == NEWLINE)
					{
						++line;
					}
					if(c == CARRTRN)
					{
						NEWLINE_TEST
					}
				}while ( c != QUOTE );
			
				/* Closing quote found. Valid String */
				/* Set the getc_offset back to the start of the string */
				b_set_getc_offset(sc_buf,lexstart);
				/* Set the str_offset attribute to the location of the current getc_offset in the str_LTBL */
				t.attribute.str_offset = b_getsize(str_LTBL);

				/* Add the characters of the string into the str_LTBL */
				for(  i = 0; i< lexend-lexstart;i++) /* Comparison of unsigned int and short will generate warning. 
														lexend-lexstart will always be positive no need to cast */
				{			
					c = b_getc(sc_buf);
					/* Ensure that quotes are not added to the string */
					if(c != '"')
					{
						if(!b_addc(str_LTBL, c))
						{
							scerrnum = FAILADDC;
							t_set_err_t(RUNTIMERR, t);
						}
					}
				}
				/* Add the string terminator to the string and set the Token Code */
				if (!b_addc(str_LTBL,STRTERM))
				{
					scerrnum = FAILADDC;
					t_set_err_t(RUNTIMERR, t);
				}
				t.code = STR_T;
				return t;
			}
	
		/* Special symbol scanning completed. Now checking for lexeme type */
		if (isalnum(c))
		{
			/* Use for loop to iterate over the transition table based on the current state and character */
			/* Continue iterating until an accepting state has been found */
			for(state = get_next_state(state,c,&accept);accept==NOAS; state = get_next_state(state,c,&accept))
			{
				c = b_getc(sc_buf);
			}
		
			/* Retract the buffer if is is an accepting state with a retract */
			if(accept==ASWR)
			{
				b_retract(sc_buf);
			}
			/* Set the end of the lexeme at the current getc_offset */
			lexend = b_get_getc_offset(sc_buf);
			/* Create a temporary buffer to store the lexeme */
			lex_buf = b_create((lexend - lexstart +1),0,'f');
			/* If buffer creation was not successful. Set the error token for a runtime error. */
			if (!lex_buf)
			{
				scerrnum = BUFFNULL;
				t_set_err_t(RUNTIMERR, t);
			}

			
			
			/* Reset the getc_offset to the start of the lexeme */
			b_set_getc_offset(sc_buf,lexstart);
			/* Add the characters of the lexeme to lex_buf */
			for( i = 0;i<lexend-lexstart;i++) /* Comparison of unsigned int and short will generate warning. 
												 lexend-lexstart will always be positive no need to cast */
			{
				if (!b_addc(lex_buf,b_getc(sc_buf)))
				{
					scerrnum = FAILADDC;
					t_set_err_t(RUNTIMERR, t);
				}
			}
			/* Pack lex_buf and add the string terminator to it */
			b_pack(lex_buf);
			/* If b_addc fails set the token for a runtime error and return t  */
			if (!b_addc(lex_buf,STRTERM))
			{
				scerrnum = FAILADDC;
				t_set_err_t(RUNTIMERR, t);
			}
			/* Call the accepting function at the current state index and pass the lexeme */
			t = aa_table[state](b_get_chmemloc(lex_buf,0));
			b_destroy(lex_buf);
			return t;
		}
		/* This code will be executed if c was an invalid symbol*/
		/* Set error token and return t. */
		t.code = ERR_T;
		t.attribute.err_lex[0] = c;
		t.attribute.err_lex[1] = STRTERM; /*Probably a better way to do this.*/
		return t;             
   }
}
/**********************************************************************************************************
Purpose:			Gets the next state in the transition table
Author:				Svillen Ranev
History/Versions:	1.0.0.0
Called functions:	char_class()
Parameters:			int state, char c, int *accept
Return value:		int representing the next row(state) in the transition table
**********************************************************************************************************/
int get_next_state(int state, char c, int *accept)
{
	int col;
	int next;
	col = char_class(c);
	next = st_table[state][col];
#ifdef DEBUG
printf("Input symbol: %c Row: %d Column: %d Next: %d \n",c,state,col,next);
#endif
       assert(next != IS);
#ifdef DEBUG
	if(next == IS){
	  printf("Scanner Error: Illegal state:\n");
	  printf("Input symbol: %c Row: %d Column: %d\n",c,state,col);
	  exit(1);
	}
#endif
	*accept = as_table[next];
	return next;
}
/**********************************************************************************************************
Purpose:				Find the column in the transition table that corresponds to the current character
Author:					Thom Palmer
History/Versions:		10.18.13
Called functions:		isalpha(), isdigit()
Parameters:				char c
Return value:			int representing a column in the transition table
Algorithm:				Find the column index, Return the column index.
**********************************************************************************************************/
int char_class (char c)
{
        int val;					/* Stores column index. */

		val = 6;
		if(isdigit(c))
		{
			switch(c)
			{
			case '0':	val = 1;
						break;
			case '8': 
			case '9':	val = 3;
						break;
			default:	val = 2;
						break;
			}
		}
		if(isalpha(c))
		{
			val = 0;
		}
		
		if(c=='.')
		{
			val = 4;
		}
		if(c=='#')
		{
			val = 5;
		}
		return val;		
}
/**********************************************************************************************************
Purpose:				Set the Token code and attribute for a keyword or arithmetic variable identifier
Author:					Chris Whitten modified by Thom Palmer
History/Versions:		10.19.13
Called functions:		iskeyword(), strlen()
Parameters:				char lexeme[]
Return value:			Token t representing a valid keyword or valid arithmetic variable identifier
Algorithm:				Create a temporary Token, if lexeme is a keyword set appropriate properties of 
						the Token and return the keyword Token, otherwise lexeme is an arithmetic variable
						identifier, set appropriate properties and return the Token
						* Assume that size_t is that same size as an int *
**********************************************************************************************************/
Token aa_func02(char lexeme[])
{
	Token t;			/* Temporary Token */
	int kwIndex;		/* Stores index in kw_table */
	int vid_offset;     /* Temporarily stores offset where vid was installed */
	
	
	/* Call iskeyword to find if lexeme is a keyword */
	kwIndex = iskeyword(lexeme);
	if( kwIndex >=0)
	{
		/* lexeme is a keyword. Set token code and attribute. */
		t.code = KW_T;
		t.attribute.kwt_idx = kwIndex;
		return t;
	}
	/* lexeme is an arithmetic variable identifier. Set appropriate code. */
	t.code = AVID_T;
	/* If lexeme is longer than VID_LEN add string terminator in lexeme at VID_LEN index */
	if (strlen(lexeme) > VID_LEN)		
	{
		lexeme[VID_LEN] = STRTERM;
	}
	/* Iterate until the end of the  lexeme and add lexeme characters to vid_lex */
	vid_offset = st_install(sym_table,lexeme, line);
	if(vid_offset < 0 )
	{
		printf("\nError: The Symbol Table is full - install failed.\n");
		st_store(sym_table);
		b_destroy(lex_buf);
		exit(SYM_TBL_FULL);
	}
	t.attribute.vid_offset = vid_offset;
	return t;
}
/**********************************************************************************************************
Purpose:				Set the Token code and attribute for a string variable identifier
Author:					Chris Whitten
History/Versions:		10.19.13
Called functions:		strlen()
Parameters:				char lexeme[]
Return value:			Token t representing a valid string variable identifier
Algorithm:				Create a temporary Token, Assign a SVID_T code to the Token, Copy the SVID to the
						vid_lex array up to 8 characters and ensure that end of string is added to the
						array. Return the token.
						* Assume that size_t is that same size as an int *
**********************************************************************************************************/
Token aa_func03(char lexeme[])
{
	Token t;			/* Temporary Token */
	int vid_offset;     /* Temporarily stores offset where vid was installed */

	/* lexeme is an string variable identifier. Set appropriate code. */
	t.code = SVID_T;
	/* If lexeme is longer than VID_LEN add string terminator in lexeme at VID_LEN and # at VID_LEN-1*/
	if (strlen(lexeme) > VID_LEN)		
	{
		lexeme[VID_LEN-1] = '#';
		lexeme[VID_LEN] = STRTERM;
	}
	vid_offset = st_install(sym_table,lexeme, line);
	if(vid_offset < 0)
	{
		printf("\nError: The Symbol Table is full - install failed.\n");
		st_store(sym_table);
		b_destroy(lex_buf);
		exit(SYM_TBL_FULL);
	}
	t.attribute.vid_offset = vid_offset;
	return t;
}
/**********************************************************************************************************
Purpose:				Set the Token code and attribute for a Decimal Integer Literal
Author:					Chris Whitten
History/Versions:		10.19.13
Called functions:		atoint(),aa_table[ES]()
Parameters:				char lexeme[]
Return value:			Token t representing a valid Decimal Integer Literal or Token t representing
						an error token
Algorithm:				Create a temporary Token. Convert the string to an integer.
						If the number is out of the valid range, call t_set_err_t() and return t. 
						Otherwise the number is valid set the Token code to INL_T and copy the number 
						to the attribute int_value. Return the token.	
						* Assume that size_t is that same size as an int *
**********************************************************************************************************/
Token aa_func05(char lexeme[])
{
	Token t;				/* Token to be returned */	
	long integerValue = 0;	/* Stores the integer value represented by lexeme */
	
	if(strlen(lexeme) > INL_LEN)
	{
		return aa_table[ES](lexeme);
	}
	/* Call decimalString_toInt to convert to lexeme to an int */
	

	integerValue = atoint(lexeme);

	/* If number is outside of the valid range we have an error state */
	if (integerValue > MAX2BYTEINT || integerValue < 0)
	{	
		/* Call the function corresponding to an error state in the accepting function table */
		return aa_table[ES](lexeme);
	}
	/* Number is valid set integer literal code and int_value attribute and return t */
	t.code = INL_T;
	t.attribute.int_value = integerValue;
	return t;
}
/**********************************************************************************************************
Purpose:				Set the Token code and attribute for a Floating Point Literal
Author:					Thom Palmer
History/Versions:		10.19.13
Called functions:		atodbl(),aa_table[ES]()
Parameters:				char lexeme[]
Return value:			Token t representing a valid Floating Point Literal or Token t representing
						an error token
Algorithm:				Create a temporary Token. Convert the string to a double. If the double is outside
						the valid range for a postive float, set the Token code to ERR_T and store the
						lexeme in the err_lex. Otherwise set the Token code to FPL_T and store the
						number as a float in the Token attribute flt_value. Return the token.
						* Assume that size_t is that same size as an int *
**********************************************************************************************************/
Token aa_func08(char lexeme[])
{
	Token t;					/* Temporary Token */
	double fplValue = 0.0;		/* Stores the floating point value represented by lexeme */
	
	fplValue = atodbl(lexeme);

	if(fplValue > FLT_MAX || (fplValue < FLT_MIN && fplValue != 0.0))
	{
		/* Call the function corresponding to an error state in the accepting function table */
		return aa_table[ES](lexeme); 
	}
	/* Number is valid set Floating Point Literal code and flt_value attribute and return t */
	t.code = FPL_T;
	t.attribute.flt_value = (float)fplValue;
	return t;
}
/**********************************************************************************************************
Purpose:				Set the Token code and attribute for a Octal Integer Literal
Author:					Thom Palmer
History/Versions:		10.19.13
Called functions:		atool(),aa_table[ES]()
Parameters:				char lexeme[]
Return value:			Token t representing a valid  Integer Literal or Token t representing
						an error token
Algorithm:				Create a temporary Token. Convert the string to an integer. If the integer 
						is outside the valid range for a postive 2 byte integer, set the Token code 
						to ERR_T and store the lexeme in the err_lex. Otherwise set the Token code
						to INL_T and store the number as an integer in the Token attribute int_value.
						Return the token.
						* Assume that size_t is that same size as an int *
**********************************************************************************************************/
Token aa_func11(char lexeme[]){

	Token t;				/* Token to be returned */
	long integerValue = 0;		/* Stores the integer value represented by lexeme */
	

	/* Convert the lexeme to int */
	integerValue = atool(lexeme);

	/* Prevent overflow errors */
	if(integerValue > MAX2BYTEINT || integerValue < 0)
	{
		/* Call the function corresponding to an error state in the accepting function table */
		return aa_table[ES](lexeme);		
	}	
	t.code = INL_T;
	t.attribute.int_value = integerValue;
	return t;
}
/**********************************************************************************************************
Purpose:				Calls t_set_err_t to set the Token code and attribute for an error token
Author:					Thom Palmer
History/Versions:		10.19.13
Called functions:		(macro)t_set_err_t()
Parameters:				char lexeme[]
Return value:			Token t representing an error token
Algorithm:				Create a temporary Token. Call t_set_err_t() to set code and attribute.
						Return the error token.
**********************************************************************************************************/
Token aa_func12(char lexeme[]){
	unsigned int i; /* Used as an iterator in the t_set_err_t macro */
	Token t;		/* Temporary token */
	t_set_err_t(lexeme,t);
}

/**********************************************************************************************************
Purpose:				Determine if the input string is a keyword or not. 
Author:					Chris Whitten
History/Versions:		10.21.13
Called functions:		strncmp()
Parameters:				char * kw_lexeme
Return value:			Index of the keyword found or -1 if a keyword is not found.
Algorithm:				Iterate through the kw_table comparing each element to the input string, if they 
						are equal return immediately. If a match is not found and the end of the table has
						been reached return -1;
**********************************************************************************************************/
int iskeyword(char * kw_lexeme)
{
	int i;		/*Used as an iterator*/

	/* Iterate through the entite kt_table*/
	for( i = 0; i < KWT_SIZE; i++)
	{
		/* Comapre the lexeme to the current point in the key word table. */
		if( strcmp(kw_lexeme, kw_table[i]) == 0)
		{
			return i;			
		}
	}
	/* Keyword not found return -1*/
	return KWNTFND;
}
/**********************************************************************************************************
Purpose:				Set the Token code and attribute for a Octal Integer Literal
Author:					Chris Whitten
History/Versions:		10.19.13
Called functions:		strlen()
Parameters:				char lexeme[]
Return value:			Integer value represented by the string.
Algorithm:				Convert each digit to an int by subtracting char '0'. For each digit multiply it by
						10 in a loop. The iterations of the loop is determined by the distance of the
						current digit from the end of the lexeme. Return the integer.					
**********************************************************************************************************/
int atoint(char lexeme[])
{
	int total = 0;	/*Store the total value */
	int digit = 0;	/*The current index of the array converted to an int*/
	unsigned int i = 0;		/*Used as an iterator*/
	unsigned int j = 0;		/*Used as an iterator*/

	/*Iterate over the full input array. */
	for(i = 0; i <strlen(lexeme); i++)
    {
		/*Convert the current index of the array to an int. */
		digit = lexeme[i] - '0';
		/*Determine the power of the current digit by finding its index in the array. */
		for(j=1; j< strlen(lexeme) -i; j++)
		{
			digit *=10;
		}
		/*Add the current digit to the total. */
		total+= digit;
	}
	return total;
}
/**********************************************************************************************************
Purpose:			Convert a lexeme representation of an Floating Pointer number to a valid to a double.
					This conversion function has been specifically designed for the platypus language and
					is not intended to be used for other purposes.
Author:				Thom Palmer
History/Versions:	10.21.13
Called functions:	strlen()
Parameters:			char lexeme[]
Return value:		Double value represented by the string
Algorithm:			Iterate over the loop to find the index location of the decimal point.
					Convert each digit to an int by subtracting char '0'. Multiply the digit by 10 
					in a loop if the index of the digit is less the decimal point index. Otherwise divide
					the digit by 10 in loop.The iterations of the loop is determined by the distance of the
					current digit from the decimal point. Return the double.
**********************************************************************************************************/
double atodbl(char lexeme[])
{
	double digit;		/* Stores value of current digit */
	double total = 0.0;
	int decimalFound = 0;	/* Used to store the index location of the decimal in the lexeme */
	int i;				/* Used to store the index of the lexeme and iterator */
	int j;				/* Used as an iterator */

	/* Iterate over the lexeme and find the index location of the decimal point/period */
	for( i = 0;i<strlen(lexeme);i++) /* Comparison of size_t and signed int will cause warning. 
										Does not affect program operation */
	{
		if(lexeme[i] == PERIOD)
		{			
			decimalFound = i;
			break;
		}
	}
	/* Foreach digit in the lexeme. */
	for( i = 0;i<strlen(lexeme);i++) /* Comparison of size_t and signed int will cause warning.
										Does not affect program operation */
	{
		/* Ignore the decimal/period */
		if(lexeme[i] == PERIOD)
		{
			continue;
		}
		/* Reset the digit to 0 */
		digit = 0.0;
		/* Subtract char '0' from the current point in the lexeme to convert it to an int.  */
		digit = (lexeme[i]-'0');
		/* Check if current index of i is to the left of the decimal point */
		if(decimalFound - i > 0)
		{
			/* Multiply the digit by 10 in a loop so that it has the correct value */
			for ( j = 1; j<decimalFound - i;j++)
			{
				digit *= 10.0;
			}
			/* Add the digit to the total */
			total += digit;
		}
		/* Check if current index of i is to the right of the decimal point */
		if(decimalFound - i < 0)
		{
			/* Divide the digit by 10 in a loop so that it has the correct value */
			for ( j = 0; j<i-decimalFound;j++)
			{
				digit /= 10.0;
			}
			/* Add the digit to the total */
			total += digit;
		}		
	} 
	return total;
}
/**********************************************************************************************************
Purpose:				Convert a lexeme representation of an Octal Integer Literal to a int. 
Author:					Thom Palmer
History/Versions:		10.21.13
Called functions:		strlen()
Parameters:				char lexeme[]
Return value:			Integer value represented by the string.
Algorithm:				Convert each digit to an int by subtracting char '0'. For each digit multiply it by
						10 in a loop. The iterations of the loop is determined by the distance of the
						current digit from the end of the lexeme. Return the integer.				
**********************************************************************************************************/
long atool(char lexeme[])
{
	long total = 0;	/*Store the total value */
	int octalDigit = 0;	/*The current index of the array converted to an int*/
	unsigned int i = 0;		/*Used as an iterator*/
	unsigned int j = 0;		/*Used as an iterator*/

	/*Iterate over the full input array. */
	for(i = 0; i <strlen(lexeme); i++)
    {
		octalDigit = 0;
		/*Convert the current index of the array to an int. */
		octalDigit = lexeme[i] - '0';
		/*Determine the power of the current digit by finding its index in the array. */
		for(j=1; j< strlen(lexeme) -i; j++)
		{
			octalDigit *=8;
		}
		/*Add the current digit to the total. */
		total+= octalDigit;
	}
	return total;
}