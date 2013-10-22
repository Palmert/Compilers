/*********************************************************************************************************
File name: table.h
Compiler: MS Visual Studio 2110
Authors: Thom Palmer - 023 713 234 and Chris Whitten - 040 611 350 
Course: CST 8152 � Compilers, Lab Section: 401
Assignment: Assignment 2 
Date: Oct. 25th 2013
Professor: Sv. Ranev
Purpose: Transition Table and function declarations necessary for the scanner implementation  
		 as required for CST8152 - Assignment #2.
Function list: a_func21(), aa_func22(), aa_func23()
*********************************************************************************************************/
#ifndef  TABLE_H_
#define  TABLE_H_ 

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif

/*   Source end-of-file (SEOF) sentinel symbol
 *    '\0' or only one of the folowing constants: 255, 0xFF , EOF
 */
#ifndef SEOF
#define SEOF(c) ((c==0xFF) || (c==EOF) || (c=='\0'))
#endif

#ifndef WHTSPC
#define WHTSPC(c) ((c==' ') || (c=='\t') || (c=='\v') || (c=='\f'))
#endif

#ifndef ISASSOP
#define is_assop(c) 	{ (c)=(b_getc(sc_buf)); if(c== ASSOP) {(t.code) = (REL_OP_T);(t.attribute.rel_op) = (EQ); return (t);} {(b_retract(sc_buf));(t.code) = (ASS_OP_T);return (t);} }
#endif

#define ASSOP   '='
#define LPRNTHS '('
#define RPRNTHS ')'
#define LBRACE  '{'
#define RBRACE  '}'
#define GRTRTHN '>'
#define LESSTHN	'<'
#define COMMA   ','
#define QUOTE   '"'
#define	SEMICLN	';'
#define NEG		'-'
#define POS		'+'
#define	ASTRX	'*'
#define FWDSLSH	'/'
#define NEWLINE '\n'

#define RUNTIMERR  "RUN TIME ERROR"

#define MAX2BYTEINT 32767


/*  Single-lexeme tokens processed separately one by one
 *  in the token-driven part of the scanner
 *   ==  !=  !<comment  <> .AND. .OR. 'wrong symbol'
 */
 

//REPLACE *ESN* WITH YOUR ERROR STATE NUMBER 
#define ES 12		/* Error state */
#define IS -1		/* Inavalid state */

/* State transition table definition */

//REPLACE *CN* WITH YOUR COLUMN NUMBER  

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
//REPLACE *N1*, *N2*, and *N3* WITH YOUR NUMBERS
#define ASWR    1  /* accepting state with retract */
#define ASNR    2 /* accepting state with no retract */
#define NOAS    3  /* not accepting state */

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

//FOR EACH OF YOUR ACCEPTING STATES YOU MUST PROVIDE
//ONE FUNCTION PROTOTYPE. THEY ALL RETURN Token AND TAKE
//ONE ARGUMENT: A string REPRESENTING A TOKEN LEXEME. 

Token aa_func02(char *lexeme); 
Token aa_func03(char *lexeme);
Token aa_func05(char *lexeme);
Token aa_func08(char *lexeme);
Token aa_func11(char *lexeme);
Token aa_func12(char *lexeme);
//Token aa_func13(char *lexeme);


//Replace XX with the number of the accepting state: 02, 03 and so on.

/* defining a new type: pointer to function (of one char * argument) 
   returning Token
*/  

typedef Token (*PTR_AAF)(char *lexeme);


/* Accepting function (action) callback table (array) definition */
/* If you do not want to use the typedef, the equvalent declaration is:
 * Token (*aa_table[])(char lexeme[]) = {
 */
//HERE YOU MUST PROVIDE AN INITIALIZATION FOR AN ARRAY OF POINTERS
//TO ACCEPTING FUNCTIONS. THE ARRAY HAS THE SAME SIZE AS as_table[ ].
//YOU MUST INITIALIZE THE ARRAY ELEMENTS WITH THE CORRESPONDING
//ACCEPTING FUNCTIONS (FOR THE STATES MARKED AS ACCEPTING IN as_table[]).
//THE REST OF THE ELEMENTS MUST BE SET TO NULL.
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
                     