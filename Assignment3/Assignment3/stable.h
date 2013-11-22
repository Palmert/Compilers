/*********************************************************************************************************
File name:		stable.h
Compiler:		MS Visual Studio 2010
Authors:		Thom Palmer - 040 713 234 and Chris Whitten - 040 611 350 
Course:			CST 8152 - Compilers, Lab Section: 401
Assignment:		Assignment 3 
Date:			November 15th, 2013
Professor:		Sv. Ranev
Purpose:		Symbol Table and function declarations necessary for the Symbol Table Implementation
				as required for CST8152 - Assignment #3.
Function list:  chk_sym_tbl()
*********************************************************************************************************/
#include "buffer.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#ifndef _STBL_H
#define _STBL_H

#define INVLD_SYM_TBL 0		/* Invalid size of symbol table */
#define ERR_SYM_TBL -1		/* Return value if symbol table is invalid */
#define SYM_TBL_FULL -1		/* Return value form the accepting funtioncs when the symbol table is full */
#define ERR_PRV_UPDTD -1	/* Return value if the STVR has previously been updated */
#define B_ADDC_FAIL -1		/* Return value if b_addc fails */
#define INVLD_OFFSET -1		/* Return value if a given offset is out of the symbol tables range */
#define	INVLD_TYPE -1		/* Return value if an invlaid type is found*/
#define SRT_SUCCESS 1		/* Return value for a successful sort */

#define I_VAL_STR -1		/* Default i_value for string data type */
#define I_VAL_FLT 0.0f		/* Default i_value for float data type */
#define I_VAL_INT 0			/* Default i_value for int data type */
#define PRV_UPDTD 1			/* Value for the status field if the STVR has previously been updated */
#define LEX_NOT_FND -1		/* Return value if lexeme was not found in the symbol table */

#define INT_TYPE 2			/* Represents an int data type */
#define FLT_TYPE 4			/* Represents a float data type*/
#define STR_TYPE 6			/* Represents a string data type*/
#define FILE_NT_FND -1		/* Return value if a file cannot be opened*/
#define FLT 'F'				/* Represents the type for a STVR that contains a float */
#define INT 'I'				/* Represents the type for a STVR that contains an int */
#define STR 'S'				/* Represents the type for a STVR that contains a string */


#ifndef CHK_SYM_TBL
#define chk_sym_tbl(sym_table) { if (sym_table.st_size == INVLD_SYM_TBL) return ERR_SYM_TBL; }
#endif

/*Bit masks*/
#define CHK_LSB     0x0001   /* 0000 0000 0000 0001 */
#define CHK_TYP		0x0006	 /* 0000 0000 0000 0110 */
#define SET_LSB     0x0001   /* 0000 0000 0000 0001 */
#define RESET12		0xFFF9   /* 1111 1111 1111 1001 */
#define SET12_01    0x0002   /* 0000 0000 0000 0010 */
#define SET12_10    0x0004   /* 0000 0000 0000 0100 */
#define SET012_111  0x0007   /* 0000 0000 0000 0111 */
#define DEFAULTZ    0x0000   /* 0000 0000 0000 0000 */
#define SETDFLT     0xFFF8   /* 1111 1111 1111 1000 */

typedef union InitialValue {
int int_val; /* integer variable initial value */
float fpl_val; /* floating-point variable initial value */
int str_offset; /* string variable initial value (offset) */
} InitialValue;

typedef struct SymbolTableVidRecord {
unsigned short status_field; /* variable record status field*/
char * plex; /* pointer to lexeme (VID name) in CA */
int o_line; /* line of first occurrence */
InitialValue i_value; /* variable initial value */
size_t ds_offset;/*offset from the beginning of data segment*/
}STVR;

typedef struct SymbolTableDescriptor {
STVR *pstvr; /* pointer to array of STVR */
int st_size; /* size in number of STVR elements */
int st_offset; /*offset in number of STVR elements */
Buffer *plsBD; /* pointer to the lexeme storage buffer descriptor */
} STD;

STD st_create(int st_size);
int st_install(STD sym_table, char *lexeme, int line);
int st_lookup(STD sym_table,char *lexeme);
int st_update_type(STD sym_table,int vid_offset,char v_type);
int st_update_value(STD sym_table, int vid_offset, InitialValue i_value);
char st_get_type (STD sym_table, int vid_offset);
void st_destroy(STD sym_table);
int st_print(STD sym_table);
int st_store(STD sym_table);
int st_sort(STD sym_table, char s_order);
int st_compare_A(const void* pstvrA, const void* pstvrB);
int st_compare_D(const void* pstvrA, const void* pstvrB);
void test_update_type(STD sym_table);

#endif
