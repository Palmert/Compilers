/*********************************************************************************************************
File name: table.h
Compiler: MS Visual Studio 2110
Author: Thom Palmer, 023 713 234 
Course: CST 8152 – Compilers, Lab Section: 231
Assignment: Assignment 2 
Date: Oct. 25th 2113
Professor: Sv. Ranev
Purpose: Transition Table and function declarations necessary for the scanner implementation  
		 as required for CST8152 - Assignment #2.
Function list:
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

/*  Single-lexeme tokens processed separately one by one
 *  in the token-driven part of the scanner
 *  '=' , ' ' , '(' , ')' , '{' , '}' , == , != , '>' , '<' ,
 *       space
 *  !<comment , ',' , '"' , ';' , '-' , '+' , '*' , '/', <> ,
 *  .AND., .OR. , SEOF, 'wrong symbol',
 */
 

//REPLACE *ESN* WITH YOUR ERROR STATE NUMBER 
#define ES -2		/* Error state */
#define IS -1		/* Inavalid state */

/* State transition table definition */

//REPLACE *CN* WITH YOUR COLUMN NUMBER  

#define TABLE_COLUMNS 9
/*transition table - type of states defined in separate table */
int  st_table[ ][TABLE_COLUMNS] = {
/* State 0 */  {  0, 1, 6, 4,-2,-2,-2,-2,23 },
/* State 1 */  {  1, 1, 1, 1,-2,-2, 3, 2,23 },
/* State 2 */  {  2,-1,-1,-1,-1,-1,-1,-1,21 },
/* State 3 */  {  3,-1,-1,-1,-1,-1,-1,-1,22 },
/* State 4 */  {  4,-2, 4, 4,-1, 7, 5, 5,23 },
/* State 5 */  {  5,-1,-1,-1,-1,-1,-1,-1,21 },
/* State 6 */  {  6,-2,10,-2, 9, 7,-2, 5,23 },
/* State 7 */  {  7,-2, 7, 7,-2,-2, 8, 8,23 },
/* State 8 */  {  8,-1,-1,-1,-1,-1,-1,-1,21 },
/* State 9 */  {  9,-2, 9,-2, 9,-2,-2,11,23 },
/* State 10*/  { 10,-1,-1,-1,-1,-1,-1,-1,22 },
/* State 11*/  { 11,-1,-1,-1,-1,-1,-1,-1,21 },
/* State 12*/  { 12,-1,-1,-1,-1,-1,-1,-1,22 },
/* State 13*/  { 13,-1,-1,-1,-1,-1,-1,-1,21 }
};

 
/* Accepting state table definition */
//REPLACE *N1*, *N2*, and *N3* WITH YOUR NUMBERS
#define ASWR    21  /* accepting state with retract */
#define ASNR    22 /* accepting state with no retract */
#define NOAS    23  /* not accepting state */

int as_table[ ] = { ASWR, ASNR, NOAS };

/* Accepting action function declarations */

//FOR EACH OF YOUR ACCEPTING STATES YOU MUST PROVIDE
//ONE FUNCTION PROTOTYPE. THEY ALL RETURN Token AND TAKE
//ONE ARGUMENT: A string REPRESENTING A TOKEN LEXEME. 

Token aa_func21(char *lexeme); 
Token aa_func22(char *lexeme);
Token aa_func23(char *lexeme);


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
	aa_func21,
	aa_func22,
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
                     