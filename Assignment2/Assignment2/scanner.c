/*********************************************************************************************************
File name:		scanner.c
Compiler:		MS Visual Studio 2110
Authors:		Thom Palmer - 023 713 234 and Chris Whitten - 040 611 350 
Course:			CST 8152 – Compilers, Lab Section: 401
Assignment:		Assignment 2 
Date:			Oct. 25th 2013
Professor:		Sv. Ranev
Version:		1.0.0.0
Purpose:		Functions implementing a Lexical Analyzer (Scanner) as required for CST8152, Assignment #2
Function list: 
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

#define DEBUG  /* for conditional processing */
#undef  DEBUG

/* Global objects - variables */
/* This buffer is used as a repository for string literals.It is defined in platy_st.c */
extern Buffer * str_LTBL; /*String literal table */
int line; /* current line number of the source code */
extern int scerrnum;     /* defined in platy_st.c - run-time error number */

/* Local(file) global objects - variables */
static Buffer *lex_buf;/*pointer to temporary lexeme buffer*/

/* No other global variable declarations/definitiond are allowed */

/* scanner.c static(local) function  prototypes */ 
static int char_class(char c); /* character class function */
static int get_next_state(int, char, int *); /* state machine function */
static int iskeyword(char * kw_lexeme); /*keywords lookup functuion */
static long atool(char * lexeme); /* converts octal string to decimal value */

/* local helper functions. */
int decimalString_toInt(char lexeme[]);
void t_set_err_t(char lexeme[], Token* t);
int octalstring_toInt(char lexeme[]);

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
   Token t; /* token to return after recognition */
   unsigned char c; /* input symbol */
   int state = 0; /* initial state of the FSM */
   short lexstart;  /*start offset of a lexeme in the input buffer */
   short lexend;    /*end   offset of a lexeme in the input buffer */
   int accept = NOAS; /* type of state - initially not accepting */  
   int i;			/*Used throughout the function as iterator*/
   char tempString[5];	/*Used to store the characters read in after '.' is foud to determine if its a logical operator*/
   
	/*Ensure the buffer is not null before trying to access it*/
   if(sc_buf == NULL)
   {
	   t.code = ERR_T;
	   strcpy(t.attribute.err_lex, RUNTIMERR);
	   return t;
   }
     
                
	while (1){ /* endless loop broken by token returns it will generate a warning */
                
    /*Set mark before getting the next character. This prevents the need for unneccessary decrements.  */
    b_setmark(sc_buf, b_get_getc_offset(sc_buf));
	lexstart = b_getmark(sc_buf);
	
	/*Get the next char from the buffer. */
	c = b_getc(sc_buf); 	
	
	/*Ensure SEOF has not been read before processing any further.*/
	if(SEOF(c))
	{
		t.code = SEOF_T;
		return t;
	}

	/*If it's whitespace ignore and return to the start of the loop.*/
	if(WHTSPC(c))
	{
		continue;
	}

	/*Drop into switch statement to determine what the character can potentially represent and handle it appropriatly. */
	switch(c)
	{
		/* if */
		case ASSOP:
			is_assop(c);
		/*If the token starts wiht ! it can either be a comment or the != relational operator, so peak forward and act appropriatly. */
		case '!':
			c = b_getc(sc_buf);
			/*If the next token is < then we have a comment therfore ignore everything in the line.*/
			if(c == LESSTHN)
			{
			do
				{
					c = b_getc(sc_buf);
				}while ( c != NEWLINE);

				++line;
				continue;
			}
			/*If the next token we have the NE relational operator. */
		   if(c == '=')
		   {
			   t.code = REL_OP_T;
			   t.attribute.rel_op = NE ;
			   return t;
		   }
		   /* if the next char is neither = or < we have an error set ERR_T*/
		   t.code = ERR_T;
		   t.attribute.err_lex[0] = '!';
		   t.attribute.err_lex[1] = c;
		   t.attribute.err_lex[2] = '\0';
	   
		   /*We assume the error was ment to be a token so ignore everything in the line. */
		   do
		   {
			   c = b_getc(sc_buf);
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
			tempString[0] = c; 
			c = b_getc(sc_buf);
			tempString[1] = c;
			/*Add characters to the temp string in order to compare them to the string constants .AND. & .OR.*/
			for (i = 2; i < 5; i++)
			{
				tempString[i] = b_getc(sc_buf);
			}
			/* Switch on the first character read after the period '.' */
			switch (c) {
				/* If its an 'A' we might have .AND. */
			case'A' :
				/* Comapre the string the string read from the buffer to the string literal .AND.  */
				if( strncmp(tempString,".AND.", 5)==0)
				{
					t.code = LOG_OP_T;
					t.attribute.log_op = AND;
					return t;
				}
			case'O':
				/* Comapre the string the string read from the buffer to the string literal .OR.  */
				if( strncmp(tempString,".OR.", 4)==0)
				{
					t.code = LOG_OP_T;
					t.attribute.log_op = OR;
					/* Retract the buffer because one extra char was taken for the .AND. comparison. */
					b_retract(sc_buf);
					return t;
				}
				/* Default case retract the buffer to the it's state prior to reading in the temp string. */
			default:
				b_set_getc_offset(sc_buf, lexstart);
				t.code = ERR_T;
				/* Add char which caused the error to the err_lex */
				t.attribute.err_lex[0] = b_getc(sc_buf);
				t.attribute.err_lex[1] = '\0';
				return t;
			}

			/* If we have an astrix sign '*' set the token and it's attirbute then return. */
		case ASTRX:
			t.code = ART_OP_T;
			t.attribute.arr_op =MULT;
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
			for ( c = b_getc(sc_buf); c!= QUOTE; c = b_getc(sc_buf))
			{
				lexend = b_get_getc_offset(sc_buf);
				/* If eob has be set or SEOF is read in from the buffer prior to closing the string */
				/* Break into the error token setup. */
				if(b_eob(sc_buf) || SEOF(c))
				{	
					/* Set the getc_offset to the start of the string */
					b_set_getc_offset(sc_buf,lexstart);
					/* Iterate over the buffer and copy the contents of the error string into the err_lex */
					for( i = 0; i < lexend-lexstart; i++)
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
								t.attribute.err_lex[i] = DECIMAL;
							}
							/* Copy a string terminator into the last index of err_lex */
							if (i==ERR_LEN)
							{
								t.attribute.err_lex[i]= '\0';
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
			}
			
			/* Closing quote found. Valid String */
			/* Set the getc_offset back to the start of the string */
			b_set_getc_offset(sc_buf,lexstart);
			/* Set the str_offset attribute to the location of the current getc_offset in the str_LTBL */
			t.attribute.str_offset = b_get_getc_offset(str_LTBL);

			/* Add the characters of the string into the str_LTBL */
			for(  i = 0; i<lexend-lexstart;i++)
			{			
				c = b_getc(sc_buf);
				/* Ensure that quotes are not added to the string */
				if(c != '"')
				{
					b_addc(str_LTBL,c);
				}
			}
			/* Add the string terminator to the string and set the Token Code */
			b_addc(str_LTBL,'\0');
			t.code = STR_T;
			return t;
		}
	

	if (isalnum(c))
	{

		for(state = get_next_state(state,c,&accept);accept==NOAS; state = get_next_state(state,c,&accept))
		{
			c = b_getc(sc_buf);
		}
			
		lex_buf = b_create(100,1,'a');
		if(accept==ASWR)
		{
			b_retract(sc_buf);
		}
		lexend = b_get_getc_offset(sc_buf);
		b_set_getc_offset(sc_buf,lexstart);
		for( i = 0;i<lexend-lexstart;i++)
		{
			b_addc(lex_buf,b_getc(sc_buf));
		}
		b_pack(lex_buf);
		b_addc(lex_buf,'\0');
		t = aa_table[state](lex_buf->ca_head);
		b_destroy(lex_buf);
		++line;
		return t;
	}
	t.code = ERR_T;
	t.attribute.err_lex[0] = c;
	t.attribute.err_lex[1] = '\0'; /*Probably a better way to do this.*/
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
/*
The assert(int test) macro can be used to add run-time diagnostic to programs
and to "defend" from producing unexpected results.
assert() is a macro that expands to an if statement;
if test evaluates to false (zero) , assert aborts the program
(by calling abort()) and sends the following message on stderr:

Assertion failed: test, file filename, line linenum

The filename and linenum listed in the message are the source file name
and line number where the assert macro appears.
If you place the #define NDEBUG directive ("no debugging")
in the source code before the #include <assert.h> directive,
the effect is to comment out the assert statement.
*/
       assert(next != IS);

/*
The other way to include diagnostics in a program is to use
conditional preprocessing as shown bellow. It allows the programmer
to send more details describing the run-time problem. 
Once the program is tested thoroughly #define DEBUG is commented out
or #undef DEBUF is used - see the top of the file.
*/ 
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
Called functions:		iskeyword(), strncpy(), strlen()
Parameters:				char lexeme[]
Return value:			Token t representing a valid keyword or valid arithmetic variable identifier
Algorithm:				Create a temporary Token, if lexeme is a keyword set appropriate properties of 
						the Token and return the keyword Token, otherwise lexeme is an arithmetic variable
						identifier, set appropriate properties and return the Token
**********************************************************************************************************/
Token aa_func02(char lexeme[])
{
	Token t;
	int kwIndex = iskeyword(lexeme);
	if( kwIndex >=0)
	{
		t.code = KW_T;
		t.attribute.kwt_idx = kwIndex;
		return t;
	}
	t.code = AVID_T;
	strncpy(t.attribute.vid_lex, lexeme, VID_LEN);	
	t.attribute.vid_lex[VID_LEN] = '\0';
	if (strlen(lexeme) < VID_LEN+1)		
	{
		t.attribute.vid_lex[strlen(lexeme)] = '\0';
	}
	return t;
}
/**********************************************************************************************************
Purpose:				Set the Token code and attribute for a string variable identifier
Author:					Chris Whitten
History/Versions:		10.19.13
Called functions:		strncpy(), strlen()
Parameters:				char lexeme[]
Return value:			Token t representing a valid string variable identifier
Algorithm:				Create a temporary Token, Assign a SVID_T code to the Token, Copy the SVID to the
						vid_lex array up to 8 characters and ensure that end of string is added to the
						array. Return the token.
**********************************************************************************************************/
Token aa_func03(char lexeme[])
{
	Token t;	
	t.code = SVID_T;	
	strncpy(t.attribute.vid_lex, lexeme, VID_LEN);
	t.attribute.vid_lex[VID_LEN-1] = '#';
	t.attribute.vid_lex[VID_LEN] = '\0';
	if (strlen(lexeme) < VID_LEN+1)		
	{
		t.attribute.vid_lex[strlen(lexeme)-1] = '#';
		t.attribute.vid_lex[strlen(lexeme)] = '\0';
	}
	return t;
}
/**********************************************************************************************************
Purpose:				Set the Token code and attribute for a Decimal Integer Literal
Author:					Chris Whitten
History/Versions:		10.19.13
Called functions:		strlen()
Parameters:				char lexeme[]
Return value:			Token t representing a valid Decimal Integer Literal or Token t representing
						an error token
Algorithm:				Create a temporary Token. Convert the string to an integer.
						If the number is out of the valid range, call t_set_err_t() and return t. 
						Otherwise the number is valid set the Token code to INL_T and copy the number 
						to the attribute int_value. Return the token.			
**********************************************************************************************************/
Token aa_func05(char lexeme[])
{
	Token t;		/*Token to be returned*/	
	int number = 0;	/*Store the total value */
	int digit = 0;	/*The current index of the array converted to an int*/
	unsigned int i = 0;		/*Used as an iterator*/
	unsigned int j = 0;		/*Used as an iterator*/

	/*Iterate ove the full input array. */
	for(i = 0; i <strlen(lexeme); i++)
    {
		/*Convert the current index of the array to an int. */
		digit = lexeme[i] - '0';
		/*Determine the power of the current digit by finding its index in the array. */
		for(j=1; j< strlen(lexeme) -i; j++)
		{
			digit *=10;
		}
		/*Add the current digit to the number. */
		number += digit;
	}

	if(number > MAX2BYTEINT || number < 0)
	{
		t_set_err_t(lexeme,&t);
		return t;
	}
	
	t.code = INL_T;
	t.attribute.int_value = number;
	return t;
}
/**********************************************************************************************************
Purpose:				Set the Token code and attribute for a Floating Point Literal
Author:					Thom Palmer
History/Versions:		10.19.13
Called functions:		strlen(), t_set_err_t()
Parameters:				char lexeme[]
Return value:			Token t representing a valid Floating Point Literal or Token t representing
						an error token
Algorithm:				Create a temporary Token. Convert the string to a double. If the double is outside
						the valid range for a postive float, set the Token code to ERR_T and store the
						lexeme in the err_lex. Otherwise set the Token code to FPL_T and store the
						number as a float in the Token attribute flt_value. Return the token.
**********************************************************************************************************/
Token aa_func08(char lexeme[])
{
	Token t;
	double digit = 0.0f;
	double total = 0.0f;
	int i;
	int j;
	int decimalFound = 0;

	for( i = 0;i<strlen(lexeme);i++)
	{
		if(lexeme[i] == '.')
		{			
			decimalFound= i;
			continue;
		}
	}
	for( i = 0;i<strlen(lexeme);i++)
	{
		if(lexeme[i] == '.')
			continue;
		digit = 0.0;		
		digit = (lexeme[i]-'0');
		if(decimalFound - i > 0)
		{
			for ( j = 1; j<decimalFound-i;j++)
			{
				digit *=10.0;
			}
			total += digit;
		}
		if(decimalFound -i<0)
		{
			for ( j = 0; j<i-decimalFound;j++)
			{
				digit /=10.0;
			}
			total += digit;
		}

	}

	
	if(total > FLT_MAX || (total < FLT_MIN && total != 0.0))
	{
		t_set_err_t(lexeme,&t);
		return t;
	}

	t.code = FPL_T;
	t.attribute.flt_value = (float)total;
	return t;
}
/**********************************************************************************************************
Purpose:				Set the Token code and attribute for a Octal Integer Literal
Author:					Thom Palmer
History/Versions:		10.19.13
Called functions:		strlen(), t_set_err_t()
Parameters:				char lexeme[]
Return value:			Token t representing a valid Octal Integer Literal or Token t representing
						an error token
Algorithm:				Create a temporary Token. Convert the string to an integer. If the integer 
						is outside the valid range for a postive 2 byte integer, set the Token code 
						to ERR_T and store the lexeme in the err_lex. Otherwise set the Token code
						to INL_T and store the number as an integer in the Token attribute int_value.
						Return the token.
**********************************************************************************************************/
Token aa_func11(char lexeme[]){

	Token t;		/*Token to be returned*/
	int total = 0;	/*Store the total value */
	int octalDigit = 0;	/*The current index of the array converted to an int*/
	unsigned int i = 0;		/*Used as an iterator*/
	unsigned int j = 0;		/*Used as an iterator*/

	/*Iterate over the full input array. */
	for(i = 0; i <strlen(lexeme); i++)
    {
		/*Convert the current index of the array to an int. */
		octalDigit = lexeme[i] - '0';
		/*Determine the power of the current digit by finding its index in the array. */
		for(j=1; j< strlen(lexeme) -i; j++)
		{
			/*Base 8 for octal digits*/
			octalDigit *=8;
		}
		/*Add the current digit to the total. */
		total+= octalDigit;
	}

	if(total > MAX2BYTEINT || total < 0)
	{
		t_set_err_t(lexeme,&t);
		return t;
	}
	
	t.code = INL_T;
	t.attribute.int_value = total;
	return t;
}
/**********************************************************************************************************
Purpose:				Calls t_set_err_t to set the Token code and attribute for an error token
Author:					Thom Palmer
History/Versions:		10.19.13
Called functions:		t_set_err_t()
Parameters:				char lexeme[]
Return value:			Token t representing an error token
Algorithm:				Create a temporary Token. Call t_set_err_t() to set code and attribute.
						Return the error token.
**********************************************************************************************************/
Token aa_func12(char lexeme[]){
	Token t;
	t_set_err_t(lexeme,&t);
	return t;
}
/**********************************************************************************************************
Purpose:				Set the Token code and attribute for a Octal Integer Literal
Author:					Thom Palmer
History/Versions:		10.19.13
Called functions:		strlen(), t_set_err_t()
Parameters:				char lexeme[]
Return value:			Token t representing a valid Octal Integer Literal or Token t representing
						an error token
Algorithm:				Create a temporary Token. Convert the string to an integer. If the integer 
						is outside the valid range for a postive 2 byte integer, set the Token code 
						to ERR_T and store the lexeme in the err_lex. Otherwise set the Token code
						to INL_T and store the number as an integer in the Token attribute int_value.
**********************************************************************************************************/
int decimalString_toInt(char lexeme[])
{
	int total = 0;	/*Store the total value */
	int digit = 0;	/*The current index of the array converted to an int*/
	unsigned int i = 0;		/*Used as an iterator*/
	unsigned int j = 0;		/*Used as an iterator*/

	/*Iterate ove the full input array. */
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
Purpose:				Convert a lexeme representation of an Octal Inter Literal to a valid 2 byte int. 
Author:					Thom Palmer
History/Versions:		10.21.13
Called functions:		none
Parameters:				char * kw_lexeme[]
Return value:			int If the input isn't a ketword -1 is returned. Otherwise the index vlause of the
						keyword in the kw_table is returned. 

Algorithm:				Iterate through the kw_table comparing each element to the input string, if they 
						are equal return immediately. If a match is not found and the end of the table has
						been reached return -1;
**********************************************************************************************************/
int octalstring_toInt(char lexeme[])
{
	int total = 0;	/*Store the total value */
	int octalDigit = 0;	/*The current index of the array converted to an int*/
	unsigned int i = 0;		/*Used as an iterator*/
	unsigned int j = 0;		/*Used as an iterator*/

	/*Iterate ove the full input array. */
	for(i = 0; i <strlen(lexeme); i++)
    {
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

/**********************************************************************************************************
Purpose:				Determine if the input string is a keyword or not. 
Author:					Thom Palmer
History/Versions:		10.21.13
Called functions:		strncpy(), strlen()
Parameters:				char * kw_lexeme
Return value:			int If the input isn't a ketword -1 is returned. Otherwise the index vlause of the
						keyword in the kw_table is returned. 

Algorithm:				Iterate through the kw_table comparing each element to the input string, if they 
						are equal return immediately. If a match is not found and the end of the table has
						been reached return -1;
**********************************************************************************************************/
int iskeyword(char * kw_lexeme)
{
	int i;
	for( i = 0; i < KWT_SIZE; i++)
	{
		if( strcmp(kw_lexeme, kw_table[i]) ==0)
		{
			return i;			
		}
	}
	i = -1;
	return i;
}
/**********************************************************************************************************
Purpose:				Set the Token to error and set the err_lex to the input lexeme. 
Author:					Thom Palmer
History/Versions:		10.21.13
Called functions:		strncpy(), strlen()
Parameters:				char lexeme[], Token *t
Return value:			None
Algorithm:				Set the Token code to ERR_t then set the err_lex attribute to the input lexeme. 
**********************************************************************************************************/
void t_set_err_t(char lexeme[], Token *t)
{
	t->code = ERR_T;
	strncpy(t->attribute.err_lex, lexeme, ERR_LEN);
	t->attribute.err_lex[ERR_LEN] = '\0';
	if(strlen(lexeme) <= ERR_LEN)
	{
		t->attribute.err_lex[strlen(lexeme)];
	}
}