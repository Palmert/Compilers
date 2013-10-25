/*********************************************************************************************************
File name:		table.h
Compiler:		MS Visual Studio 2110
Authors:		Thom Palmer - 040 713 234 and Chris Whitten - 040 611 350 
Course:			CST 8152 - Compilers, Lab Section: 401
Assignment:		Assignment 2 
Date:			Oct.25th 2013
Professor:		Sv. Ranev
Purpose:		Transition Table and function declarations necessary for the scanner implementation  
				as required for CST8152 - Assignment #2.
Function list:	t_set_err_t()
*********************************************************************************************************/
#ifndef  TABLE_H_
#define  TABLE_H_ 

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif

#ifndef SEOF
/* Used to check for all possible SEOF values rather than manually checking each one in every conditional statement, this macro will work for both 
signed and unsigned chars. */
#define SEOF(c) ((c)==255 || (c)==EOF || (c)=='\0')
#endif

#ifndef WHTSPC
/* Used to check for all possible whitespace characters, rather than having to manually check each one in every conditional statement in scanner.c.
1 will be returned if any whitespace character is found.*/
#define WHTSPC(c) ((c)==' ' || c=='\t' || (c)=='\v' || (c)=='\f')
#endif

#ifndef T_SET_ERR_T
/**********************************************************************************************************
Purpose:				Set the Token to error and copy the lexeme to the error lex
Author:					Chris Whitten
History/Versions:		10.21.13
Called functions:		strlen()
Parameters:				char lexeme[], Token *t
Return value:			Error Token
Algorithm:				Set the Token code to ERR_t then set the err_lex attribute to the input lexeme. 
						* Assume that size_t is that same size as an int *
**********************************************************************************************************/
#define t_set_err_t(lexeme, t){ \
								(t).code = ERR_T; \
								if(ERR_LEN+1 < strlen((lexeme))) \
									(t).attribute.err_lex[ERR_LEN] = STRTERM; \
								for(i=0;i<ERR_LEN && i<=strlen(lexeme);i++) \
									(t).attribute.err_lex[i]=(lexeme)[i]; \
								return (t); \
							  }					
#endif


#define EQSIGN    '='		/* Equal sign symbol constant */
#define LPRNTHS  '('		/* Left parenthesis symbol constant */
#define RPRNTHS  ')'		/* Right parenthesis symbol constant */
#define LBRACE   '{'		/* Left brace symbol constant */
#define RBRACE   '}'		/* Right brace symbol constant */
#define GRTRTHN  '>'		/* Greater than symbol constant */
#define LESSTHN	 '<'		/* Less than symbol constant */
#define COMMA    ','		/* Comma symbol constant */
#define QUOTE    '"'		/* Quotation mark symbol constant */
#define	SEMICLN	 ';'		/* Semicolon symbol constant */
#define NEG		 '-'		/* Minus symbol sign constant */
#define POS		 '+'		/* Plus sign symbol constant */
#define	ASTRX	 '*'		/* Asterix symbol constant */
#define FWDSLSH	 '/'		/* Forward slash symbol constant */
#define NEWLINE  '\n'		/* Newline symbol constant */
#define PERIOD	 '.'		/* Period symbol constant */
#define STRTERM	 '\0'		/* String terminator constant */
#define EXCLAMTN '!'		/* Exclamation point symbol constant */
#define CARRTRN	 '\r'		/* Carriage return symbol constant */
#define LOG_OP_AND ".AND."	/* Used to represent the logical operator .AND. string*/
#define LOG_OP_OR  ".OR."	/* Used to represent the logical operator .OR. string*/

#define RUNTIMERR  "RUN TIME ERROR" /* Constant String for run time errors */
#define KWNTFND -1			/* Return value for iskeyword if keyword is not found */

#define MAX2BYTEINT 32767			/* Constant for a 2 byte int */

 
#define ES 12		/* Error state */
#define IS -1		/* Inavalid state */

/* State transition table definition */
#define TABLE_COLUMNS 7
/*transition table - type of states defined in separate table */
int  st_table[ ][TABLE_COLUMNS] = {
/* State 0 */  {  1, 6, 4, 4,ES,ES,ES },
/* State 1 */  {  1, 1, 1, 1,2, 3, 2 },
/* State 2 */  { IS,IS,IS,IS,IS,IS,IS },
/* State 3 */  { IS,IS,IS,IS,IS,IS,IS },
/* State 4 */  { ES, 4, 4, 4, 7, 5, 5 },
/* State 5 */  { IS,IS,IS,IS,IS,IS,IS },
/* State 6 */  { ES,10, 9,ES, 7,ES, 5 },
/* State 7 */  { 8, 7, 7, 7, 8, 8, 8 },
/* State 8 */  { IS,IS,IS,IS,IS,IS,IS },
/* State 9 */  { ES, 9, 9,ES,ES,ES,11 },
/* State 10*/  { ES,ES,ES,ES,ES,ES,11 },
/* State 11*/  { IS,IS,IS,IS,IS,IS,IS },
/* State 12*/  { IS,IS,IS,IS,IS,IS,IS },
/* State 13*/  { IS,IS,IS,IS,IS,IS,IS }
};

 
/* Accepting state table definition */
#define ASWR    1	/* accepting state with retract */
#define ASNR    2	/* accepting state with no retract */
#define NOAS    3	/* not accepting state */

int as_table[ ] = { 
	NOAS, 
	NOAS, 
	ASWR, 
	ASNR, 
	NOAS, 
	ASWR, 
	NOAS, 
	NOAS, 
	ASWR, 
	NOAS, 
	NOAS, 
	ASWR, 
	ASNR, 
	ASWR 
};

/* Accepting action function declarations */
Token aa_func02(char *lexeme);		
Token aa_func03(char *lexeme);
Token aa_func05(char *lexeme);
Token aa_func08(char *lexeme);
Token aa_func11(char *lexeme);
Token aa_func12(char *lexeme);

/* Defining a new type: pointer to function (of one char * argument) returning Token */
typedef Token (*PTR_AAF)(char *lexeme);

/* Accepting function (action) callback table (array) definition */
PTR_AAF aa_table[ ] = {
	NULL,
	NULL,
	aa_func02,
	aa_func03,
	NULL,
	aa_func05,
	NULL,
	NULL,
	aa_func08,
	NULL,
	NULL,
	aa_func11,
	aa_func12,
	NULL
};

/* Keyword lookup table (.AND. and .OR. are not keywords) */
#define KWT_SIZE  8

char * kw_table []= {
                      "ELSE",
                      "IF",
                      "INPUT",
                      "OUTPUT",
                      "PLATYPUS",
                      "REPEAT",
                      "THEN",
                      "USING"   
                     };

#endif
                     