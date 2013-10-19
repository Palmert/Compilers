/*********************************************************************************************************
File name: scanner.c
Compiler: MS Visual Studio 2110
Authors: Thom Palmer - 023 713 234 and Chris Whitten - 040 611 350 
Course: CST 8152 – Compilers, Lab Section: 401
Assignment: Assignment 2 
Date: Oct. 25th 2013
Professor: Sv. Ranev
Purpose: Functions implementing a Lexical Analyzer (Scanner) as required for CST8152, Assignment #2
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
/* This buffer is used as a repository for string literals.
   It is defined in platy_st.c */
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

int scanner_init(Buffer * sc_buf) {
  	if(b_isempty(sc_buf)) return EXIT_FAILURE;/*1*/
	b_set_getc_offset(sc_buf,0);/* in case the buffer has been read previously  */
	b_reset(str_LTBL);
	line = 1;
	return EXIT_SUCCESS;/*0*/
/*   scerrnum = 0;  *//*no need - global ANSI C */
}

Token mlwpar_next_token(Buffer * sc_buf)
{
   Token t; /* token to return after recognition */
   unsigned char c; /* input symbol */
   int state = 0; /* initial state of the FSM */
   short lexstart;  /*start offset of a lexeme in the input buffer */
   short lexend;    /*end   offset of a lexeme in the input buffer */
   int accept = NOAS; /* type of state - initially not accepting */  
/* 
lexstart is the offset from the beginning of the char buffer of the
input buffer (sc_buf) to the first character of the current lexeme,
which is being processed by the scanner.
lexend is the offset from the beginning of the char buffer of the
input buffer (sc_buf) to the last character of the current lexeme,
which is being processed by the scanner.

*/ 
        
        
        //DECLARE YOUR VARIABLES HERE IF NEEDED 
        
                
	while (1){ /* endless loop broken by token returns it will generate a warning */
                
       // GET THE NEXT SYMBOL FROM THE INPUT BUFFER 
        
	c = b_getc(sc_buf); 
	

   switch(c)
   {
	case '=':
		c = b_getc(sc_buf);
		if(c == '=')
		{
		t.code = REL_OP_T;
		t.attribute.rel_op = EQ;
		return t;
		}
		b_retract(sc_buf);
		t.code = ASS_OP_T;
		return t;
	/*If the token starts wiht ! it can either be a comment or the != relational operator, so peak forward and act appropriatly. */
	case '!':
		c = b_getc(sc_buf);
		/*If the next token is < then we have a comment and ifnot everything till the newline character is hit. */
		if(c == '<')
	    {
			do
		    {
				c = b_getc(sc_buf);
		    }while ( c != '\n');
			// If output doesn't match revert to his logic as follows
			// IF (c == '!') TRY TO PROCESS COMMENT
			//IF THE FOLLOWING IS NOT CHAR IS NOT < REPORT AN ERROR
			//ELSE IN A LOOP SKIP CHARACTERS UNTIL \n THEN continue;
			t.code = COM_T;
			++line;
			return t ;
	   }
		/*If it's the != relation operator set the proper values and return.  */
	   if(c == '=')
	   {
		   t.code = REL_OP_T;
		   t.attribute.rel_op = NE ;
		   return t;
	   }
	   t.code = ERR_T;


	   return t;

	case '+':
		t.code = ART_OP_T;
		t.attribute.arr_op = PLUS;
		return t;

	case '-':
		t.code = ART_OP_T;
		t.attribute.arr_op = MINUS;
		return t;
		case'.':
		char tempString[5];
		lexstart = b_get_getc_offset(sc_buf) -1;
		c = b_getc(sc_buf);

		char orString [] ={'.','O','R','.'};
		char andString[] ={'.','A','N','D','.'};

		for (int i = 0; 1 < 5; i++)
		{
			tempString[i] = b_getc(sc_buf);
		}
		switch (c) {

		case'A' :

			if( strncmp(tempString,andString, 5)==0)
			{
				t.code = LOG_OP_T;
				t.attribute.log_op = AND;
				return t;
			}
		case'O':

			if( strncmp(tempString,orString, 4)==0)
			{
				t.code = LOG_OP_T;
				t.attribute.log_op = OR;
				return t;
			}
		default:
			b_set_getc_offset(sc_buf, lexstart +1);
			t.code = ERR_T;
			return t;
		}

		return t;


	case '*':
		t.code = ART_OP_T;
		t.attribute.arr_op =MULT;
		return t;

	case'/':
		t.code = ART_OP_T;
		t.attribute.arr_op = DIV;
		return t;

	case'{':
		t.code = RBR_T;
		return t;

	case'}':
		t.code = LBR_T;
		return t;

	case '(':
		t.code = LPR_T;
		return t;

	case ')':
		t.code = RPR_T;
		return t;

	case '<':
		c = b_getc(sc_buf);

		if(c == '>'){
			t.code = SCC_OP_T;
			return t;
		}
		t.code = REL_OP_T;
		t.attribute.rel_op = LT;
		b_retract(sc_buf);
		return t;

	case '>':
		t.code = REL_OP_T;
		t.attribute.rel_op = GT;
		return t;

	case '\n':
		++line;
		continue;

	case ',':
		t.code = COM_T;  //   SEOF, 'wrong symbol',
		return t;

	
		t.code = EOS_T;
		return t;

	case'"':
		//need to handle \n within string
		b_setmark(sc_buf, b_get_getc_offset(sc_buf));
		lexstart = b_getmark(sc_buf);
		for(c = b_getc(sc_buf);c!='"';c = b_getc(sc_buf))
		{
			lexend = b_get_getc_offset(sc_buf);
			if(b_eob(sc_buf))
			{				
				b_set_getc_offset(sc_buf,lexstart);
				for(int i = 0; i<lexend;i++)
				{
					if(i==ERR_LEN+1)
					{
						break;
					}
					c = b_getc(sc_buf);
					t.attribute.err_lex[i] = c;
					if(i>=17)
					{
						t.attribute.err_lex[i] = '.';
					}
				}
				t.code = ERR_T;
				return t;
			}
		}
		b_set_getc_offset(sc_buf,lexstart);
		t.attribute.str_offset = str_LTBL->addc_offset;
		for( int i = 0; i<lexend;i++)
		{
			c = b_getc(sc_buf);
			b_addc(str_LTBL,c);
		
		}
		b_addc(str_LTBL,'\0');
		t.code = STR_T;
		return t;

	case ' ':
		continue;

	}
	if(SEOF(c))
	{
		t.code = SEOF_T;
		return t;
	}
	if (isalpha(c))
	{
		b_setmark(sc_buf,b_get_getc_offset(sc_buf));
		state = get_next_state(state,c,&accept);
		for(b_getc(sc_buf);accept==NOAS;b_getc(sc_buf))
		{
			state = get_next_state(state,c,&accept);
		}
		lexstart = b_getmark(sc_buf)-1;
		lex_buf = b_create(b_get_getc_offset(sc_buf) - lexstart,1,'a');
		if(state==ASWR)
		{
			b_retract(sc_buf);
		}
		lexend = b_get_getc_offset(sc_buf);
		for(int i = 0;i<lexend-lexstart;i++)
		{
			b_addc(lex_buf,b_getc(sc_buf));
		}

		aa_table[state](lex_buf->ca_head);
		b_destroy(lex_buf);
		return t;
	}
	t.code = ERR_T;
	t.attribute.err_lex[0] = c;
	return t;             
   }
}


DO NOT MODIFY THE CODE OF THIS FUNCTION
YOU CAN REMOVE THE COMMENTS

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

int char_class (char c)
{
        int val;

THIS FUNCTION RETURNS THE COLUMN NUMBER IN THE TRANSITION
TABLE st_table FOR THE INPUT CHARACTER c.
SOME COLUMNS MAY REPRESENT A CHARACTER CLASS .
FOR EXAMPLE IF COLUMN 1 REPRESENTS [A-Z]
THE FUNCTION RETURNS 1 EVERY TIME c IS ONE
OF THE LETTERS A,B,...,Z.
        
        return val;
}



HERE YOU WRITE THE DEFINITIONS FOR YOUR ACCEPTING FUNCTIONS. 
************************************************************

ACCEPTING FUNCTION FOR THE arithmentic variable identifier AND keywords (VID - AVID/KW)
REPLACE XX WITH THE CORRESPONDING ACCEPTING STATE NUMBER

Token aa_funcXX(char lexeme[]){

WHEN CALLED THE FUNCTION MUST
1. CHECK IF THE LEXEME IS A KEYWORD.
   IF YES, IT MUST RETURN A TOKEN WITH THE CORRESPONDING ATTRIBUTE
   FOR THE KEYWORD. THE ATTRIBUTE CODE FOR THE KEYWORD
   IS ITS INDEX IN THE KEYWORD LOOKUP TABLE (kw_table in table.h).
   IF THE LEXEME IS NOT A KEYWORD, GO TO STEP 2.

2. SET a AVID TOKEN.
   IF THE lexeme IS LONGER than VID_LEN (see token.h) CHARACTERS,
   ONLY FIRST VID_LEN CHARACTERS ARE STORED 
   INTO THE VARIABLE ATTRIBUTE ARRAY vid_lex[](see token.h) .
   ADD \0 AT THE END TO MAKE A C-type STRING.
  return t;
}



ACCEPTING FUNCTION FOR THE string variable identifier (VID - SVID)
REPLACE XX WITH THE CORRESPONDING ACCEPTING STATE NUMBER

Token aa_funcXX(char lexeme[]){

WHEN CALLED THE FUNCTION MUST
1. SET a SVID TOKEN.
   IF THE lexeme IS LONGER than VID_LEN characters,
   ONLY FIRST VID_LEN-1 CHARACTERS ARE STORED
   INTO THE VARIABLE ATTRIBUTE ARRAY vid_lex[],
   AND THEN THE # CHARACTER IS APPENDED TO THE NAME.
   ADD \0 AT THE END TO MAKE A C-type STRING.
  
  return t;
}

ACCEPTING FUNCTION FOR THE floating-point literal (FPL)

Token aa_funcXX(char lexeme[]){

THE FUNCTION MUST CONVERT THE LEXEME TO A FLOATING POINT VALUE,
WHICH IS THE ATTRIBUTE FOR THE TOKEN.
THE VALUE MUST BE IN THE SAME RANGE AS the value of 4-byte float in C.
IN CASE OF ERROR (OUT OF RANGE) THE FUNCTION MUST RETURN ERROR TOKEN
THE ERROR TOKEN ATTRIBUTE IS  lexeme
  return t;
}

ACCEPTING FUNCTION FOR THE integer literal(IL) - decimal constant (DIL)

Token aa_funcXX(char lexeme[]){

THE FUNCTION MUST CONVERT THE LEXEME REPRESENTING A DECIMAL CONSTANT
TO A DECIMAL INTEGER VALUE, WHICH IS THE ATTRIBUTE FOR THE TOKEN.
THE VALUE MUST BE IN THE SAME RANGE AS the value of 2-byte int in C.
IN CASE OF ERROR (OUT OF RANGE) THE FUNCTION MUST RETURN ERROR TOKEN
THE ERROR TOKEN ATTRIBUTE IS  lexeme
  return t;
}

ACCEPTING FUNCTION FOR THE integer literal(IL) - octal constant (OIL)

Token aa_funcXX(char lexeme[]){

THE FUNCTION MUST CONVERT THE LEXEME REPRESENTING AN OCTAL CONSTANT
TO A DECIMAL INTEGER VALUE WHICH IS THE ATTRIBUTE FOR THE TOKEN.
THE VALUE MUST BE IN THE SAME RANGE AS the value of 2-byte int in C.
THIS FUNCTION IS SIMILAR TO THE FUNCTION ABOVE AND THEY CAN BE
COMBINED INTO ONE FUNCTION
THE MAIN DIFFERENCE IE THAT THIS FUNCTION CALLS
THE FUNCTION atool(char * lexeme) WHICH CONVERTS AN ASCII STRING
REPRESENTING AN OCTAL NUMBER TO INTEGER VALUE
IN CASE OF ERROR (OUT OF RANGE) THE FUNCTION MUST RETURN ERROR TOKEN
THE ERROR TOKEN ATTRIBUTE IS  lexeme

  return t;
}

ACCEPTING FUNCTION FOR THE ERROR TOKEN 

Token aa_funcXX(char lexeme[]){

THE FUNCTION SETS THE ERROR TOKEN. lexeme[] CONTAINS THE ERROR
THE ATTRIBUTE OF THE ERROR TOKEN IS THE lexeme ITSELF
AND IT MUST BE STORED in err_lex.  IF THE ERROR LEXEME IS LONGER
than ERR_LEN caharacters, only the first ERR_LEN character are
stored in err_lex.

  return t;
}


CONVERSION FUNCTION

long atool(char * lexeme){

THE FUNCTION CONVERTS AN ASCII STRING
REPRESENTING AN OCTAL INTEGER CONSTANT TO INTEGER VALUE
}

HERE YOU WRITE YOUR ADDITIONAL FUNCTIONS (IF ANY).
FOR EXAMPLE

int iskeyword(char * kw_lexeme){}