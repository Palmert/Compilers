/*********************************************************************************************************
File name:		stable.c
Compiler:		MS Visual Studio 2010
Authors:		Thom Palmer - 040 713 234 and Chris Whitten - 040 611 350 
Course:			CST 8152 - Compilers, Lab Section: 401
Assignment:		Assignment 3 
Date:			Novemeber 15th 2013
Professor:		Sv. Ranev
Purpose:		Symbol Table and function definitions necessary for the Symbol Table Implementation
				as required for CST8152 - Assignment #3.
Function list:  st_create(),st_install(),st_lookup(),st_update_type(),st_update_value(),st_get_type(),
				st_destroy(),st_print,st_setsize(),st_incoffset(),st_store(),st_sort(),st_compare_A(),
				st_compare_B(),test_update_type()
*********************************************************************************************************/
#include "stable.h"

extern STD sym_table;
static void st_setsize(void);
static void st_incoffset(void);
/**********************************************************************************************************
Purpose:				Create a new Symbol Table Descriptor.
Author:					Thom Palmer
History/Versions:		10.18.13
Called functions:		malloc(), sizeof(), b_create()
Parameters:				STD sym_table,char *lexeme
Return value:			Offset of the STVR if it already exists -1 on failure.  
Algorithm:				Create a local STD, initialize the STD.st_offset to 0, allocate memory for an array 
						of STVRs with st_size STVRs, create a self incrementing buffer and point STD.plsBD 
						to its head, if either memory allocations fail set the STD.st_size to 0 
						and return the STD, otherwise set STD.st_size to st_size and return STD
**********************************************************************************************************/
STD st_create(int st_size)
{
	STD localSTD;				/* Local variable for Symbol Table Descriptor*/
	if(st_size <= 0)
	{
		localSTD.st_size = 0;
		return localSTD;
	}
	/* Set offset to 0*/
	localSTD.st_offset = 0;	
	/* Allocate dynamic memory for an array of STVRs */
	localSTD.pstvr = (STVR*)malloc(sizeof(STVR)*st_size); 
	/* Check for a failure */
	if(!localSTD.pstvr)
	{
		/* Set the size of the STD to 0 and return STD*/
		localSTD.st_size = 0;
		return localSTD;
	}

	/* Create a new buffer in additive mode */
	localSTD.plsBD = b_create(1,1,'a'); // Examine initial size of buffer and increment
	/* Check for a failure in buffer creation */
	if(!localSTD.plsBD)
	{
		/* Set the size of the STD to 0 and return STD*/
		free((STVR*) localSTD.pstvr);
		localSTD.st_size = 0;
		return localSTD;
	}

	/* Otherwise functions was successful*/
	/* Set the size of the STD to st_size and return STD*/
	localSTD.st_size = st_size;
	return localSTD;
}
/**********************************************************************************************************
Purpose:				Add a new STVR into the symbol table
Author:					Thom Palmer
History/Versions:		10.18.13
Called functions:		macro-chk_sym_tbl(), st_lookup(), b_get_chmemloc(), b_getsize(), strlen(), b_addc(),
						b_get_r_flag(),st_incoffset()
Parameters:				STD sym_table,char *lexeme
Return value:			Offset of the STVR if it already exists -1 on failure.  
Algorithm:				Check for a valid symbol table, call st_lookup to find if the lexeme is already
						in the symbol table, if it is return the offset where it was found, otherwise
						point the current plex to the current size of the buffer, initialize o_line &
						status_field, store the lexeme in the buffer using a call to b_addc, 
						if a reallocation occurs at any point adjust all of the plex pointers, determine
						the default datatype of the lexeme and set it, increment the offset and return
						STD.st_offset
						* Assume that size_t is that same size as an int *
**********************************************************************************************************/
int st_install(STD sym_table, char *lexeme, int line)
{
	int vid_offset;							/* Stores the offset where lexeme is found */
	unsigned int i;									/* Used and an iterator*/
	short offset = 0;						/* Used to store the offset from the buffer head */
	int rFlag = 0;							/* Flag indicating if a reallocation occured in the buffer */

	/*Check for valid symbol table*/
	chk_sym_tbl(sym_table);	
	/* Call st_lookup to search for the lexeme in the table */
	vid_offset = st_lookup(sym_table, lexeme);
	/* If the lexeme was found. Return the offset where it was found */
	if(vid_offset != LEX_NOT_FND )
	{
		return vid_offset;
	}
	/* Check if symbol table is full */
	if(sym_table.st_size == sym_table.st_offset)
	{
		return SYM_TBL_FULL; 
	}
	/* Set the plex point using a call to b_get_chmemloc. This is the only buffer function that returns a pointer */
	sym_table.pstvr[sym_table.st_offset].plex  = b_get_chmemloc(sym_table.plsBD, b_getsize(sym_table.plsBD));
	/* Set o_line to the current line number */
	sym_table.pstvr[sym_table.st_offset].o_line = line;
	/* Clear the status filed berfore setting the default value*/
	sym_table.pstvr[sym_table.st_offset].status_field &= DEFAULTZ;
	/* Set the status field to default values using the default bitmask */
	sym_table.pstvr[sym_table.st_offset].status_field |= SETDFLT; 
	
	/* Iterate through the lexeme and store it as a c-type string within the symbol table buffer */ 
	for( i=0;i<=strlen(lexeme);i++)
	{
		/* Store each character including the string terminator */ 
		if(!b_addc(sym_table.plsBD, lexeme[i]))
		{
			/* Returning -1 on b_addc failure. Allows program to handle failure in this function correctly */
			return B_ADDC_FAIL; 
		}
		/* Increment rFlag if it is set*/
		if(b_get_r_flag(sym_table.plsBD))
		{			
			++rFlag;			
		}			
	}
	/*  According to language specs the VID is a string if its last character is a # */
	if(lexeme[strlen(lexeme) -1] == '#')
	{
		/* Set the data type field to a string using a bitmask and set i_value to -1 */
		sym_table.pstvr[sym_table.st_offset].status_field |= SET012_111; 
		sym_table.pstvr[sym_table.st_offset].i_value.str_offset = I_VAL_STR;
	}
	else
	{		
		switch(lexeme[0])
		{
		/*  According to language specs the  type of an VID is Integer if it's first character is i, o, d, or n */
		case 'd': case 'i': case 'n': case 'o':
			/* Set the data type field to an int using a bitmask and set i_value to 0 */
			sym_table.pstvr[sym_table.st_offset].status_field |= SET12_01; 
			sym_table.pstvr[sym_table.st_offset].i_value.int_val = I_VAL_INT;
			break;

		/* According to the language specs the default type for a VID is float */
		default:
			/* Set the data type field to a float using a bitmask and set i_value to 0 */
			sym_table.pstvr[sym_table.st_offset].status_field |= SET12_10; 
			sym_table.pstvr[sym_table.st_offset].i_value.fpl_val = I_VAL_FLT;
			break;
		}
	}
	/* If the rflag is set we will need to handle reallocating plex pointers */
	if(rFlag)
	{
		/* Iterate though the array of STVRs */
		/* Cast to unsigned because the offset is and index value which cannot be negative*/
		for(i=0; i<= (unsigned)sym_table.st_offset; i++)
		{							
			/* Correct the plex pointers of each STVR using the lengths of the previous lexemes cumulatively */
			sym_table.pstvr[i].plex  = b_get_chmemloc(sym_table.plsBD, offset);
			/* Casting to a short ebcause the plex is stored in the buffer which could never be larget that SHRT_MAX*/
			offset += (short)strlen(sym_table.pstvr[i].plex)+1;
		}	
	}

	/* Increment the current st_offset and return st_offset */
	st_incoffset();
	return sym_table.st_offset;
}
/**********************************************************************************************************
Purpose:				Check if a STVR with the given lexeme already exists. 
Author:					Chris Whitten
History/Versions:		10.18.13
Called functions:		chk_sym_tbl(), strcmp()
Parameters:				STD sym_table,char *lexeme
Return value:			Offset of the STVR if it already exists -1 on failure.  
Algorithm:				Ensure a valid symbol table was given then iterate of each element comapring the 
						plex, if a match is found return the offset otherwise return -1.
**********************************************************************************************************/
int st_lookup(STD sym_table,char *lexeme)
{
	chk_sym_tbl(sym_table);
	/* Iterate over STVR array plexs to see if any of them match the lexeme */
	while((--sym_table.st_offset>0)&&strcmp(lexeme, sym_table.pstvr[sym_table.st_offset].plex));	
	/* Can use sym_table.st_offset as an iterator because it is passed by value */
	return sym_table.st_offset;
}
/**********************************************************************************************************
Purpose:				Update the type of the SVTR at the given offset. 
Author:					Chris Whitten
History/Versions:		10.18.13
Called functions:		chk_sym_tbl()
Parameters:				STD sym_table, int vid_offset,InitialValue i_value
Return value:			Offset of the updated STVR. -1 on failure. 
Algorithm:				Check for a valid symbol table and that the offset is within the size of the symbol
						table. If the table and offset are valid check if the STVR has preiously been updated, 
						if it has not been update it and set the update flag then return. otherwise return 
						a failure. 
**********************************************************************************************************/
int st_update_type(STD sym_table,int vid_offset,char v_type)
{
	chk_sym_tbl(sym_table);

	/* Ensure the offset is within range of valid STVR's */
	if(vid_offset >= sym_table.st_offset) 
	{
		return INVLD_OFFSET; 
	}

	/* Check to make sure the type has not been previously updated. */
	if((sym_table.pstvr[vid_offset].status_field & CHK_LSB) == PRV_UPDTD)
	{
		return ERR_PRV_UPDTD;
	}  
	/* Reset the flags before updating the type of the STVR */
	sym_table.pstvr[vid_offset].status_field &= RESET12;

	/* Depending on the type update the status filed accordingly.*/
	switch(v_type)
	{
		case FLT:
			sym_table.pstvr[vid_offset].status_field |= SET12_10;
			break;
		case INT:
			sym_table.pstvr[vid_offset].status_field |= SET12_01;
			break;
		default:
			return INVLD_TYPE; // Check value for INVLD_TYPE
	}

	/* Set the updated flag so the STVR cannot be updated in the future */ 
	sym_table.pstvr[vid_offset].status_field |= SET_LSB;

	return vid_offset;	
}
/**********************************************************************************************************
Purpose:				Update the value of the SVTR at the given offset. 
Author:					Chris Whitten
History/Versions:		10.18.13
Called functions:		chk_sym_tbl()
Parameters:				STD sym_table, int vid_offset,InitialValue i_value
Return value:			Offset of the updated STVR. -1 on failure. 
Algorithm:				Check for a valid symbol table and that the offset is within the size of the symbol
						table, then update the value with the given value. 
**********************************************************************************************************/
int st_update_value(STD sym_table, int vid_offset,InitialValue i_value)
{
	chk_sym_tbl(sym_table);

	/* Ensure the offset is within range of valid STVR's */
	if(vid_offset >= sym_table.st_offset) 
	{
		return INVLD_OFFSET; 
	}
	/* Update the i_value */
	sym_table.pstvr[vid_offset].i_value = i_value;
	return vid_offset;
}
/**********************************************************************************************************
Purpose:				Get the type of the STVR at the given offset. 
Author:					Chris Whitten
History/Versions:		10.18.13
Called functions:		chk_sym_tbl()
Parameters:				STD sym_table, int vid_offset
Return value:			char F if it's a float, I if it's an int, S if it's a string. -1 on failure. 
Algorithm:				Check for a valid symbol table, then compare the status field of the
						STVR at the given offset by using bit masking then return the corresponidng 
						char that represents the type. 
**********************************************************************************************************/
char st_get_type(STD sym_table, int vid_offset)
{
	chk_sym_tbl(sym_table);

	/* Ensure the offset is within range of valid STVR's */
	if(vid_offset >= sym_table.st_offset)
	{
		return INVLD_OFFSET; 
	}
	/* Switch of the status field and use a bit mask to isolate the data type bits*/
	switch(sym_table.pstvr[vid_offset].status_field & CHK_TYP )
	{
		case INT_TYPE:
			return INT;

		case FLT_TYPE:
			return FLT;

		case STR_TYPE:
			return STR;
	}
	/* Should never happen. Included to prevent not all paths return warning */
	return INVLD_TYPE;
}
/**********************************************************************************************************
Purpose:				Free all the dynamically allocated memory associated with the symbol table. 
Author:					Thom Palmer
History/Versions:		10.18.13
Called functions:		b_destroy(), free()
Parameters:				STD sym_table
Return value:			void
Algorithm:				Check to ensure both dynamically allocated stuctures have valid pointers before 
						freeing the memory. We thought this was safer then checking the size.
**********************************************************************************************************/
void st_destroy(STD sym_table)
{
	/* Even though b_destroy handles NULL this check avoids creating an unnessecary stack frame */
	if(sym_table.plsBD)
	{
		b_destroy(sym_table.plsBD);
	}
	/* Ensure the pointer is valid before freeing the memory to avoid crashing */
	if(sym_table.pstvr)
	{
		free((STVR*)sym_table.pstvr);		
	}
	/* Set the size to 0 so the STD will be marked as invalid */
	st_setsize();
}
/**********************************************************************************************************
Purpose:				Print the contents of the symbol table. 
Author:					Chris Whitten
History/Versions:		10.18.13
Called functions:		printf(), chk_sym_tbl()
Parameters:				STD sym_table
Return value:			number of elements printed on success -1 on failure.
Algorithm:				Check for a valid symbol table, then interate over every element in the table 
						printing out it's attributes. 
**********************************************************************************************************/
int st_print(STD sym_table)
{
	int elemPrintd;					/* Used as a counter to iterate over the symbol table and counts the
										number of elements printed */
	 
	chk_sym_tbl(sym_table);

	/* Print out title and header */
	printf("\nSymbol Table\n");
	printf("____________\n");
	printf("\nLine Number Variable Identifier\n");

	/* Iterate over entire symbol table printing the curren STVR's attributes */
	for(elemPrintd=0;elemPrintd<sym_table.st_offset; elemPrintd++)
	{
		if(sym_table.pstvr[elemPrintd].plex != NULL)
		{
			printf("%2d%10c%s\n", sym_table.pstvr[elemPrintd].o_line, ' ', sym_table.pstvr[elemPrintd].plex);
		}
	}
	return elemPrintd;
}
/**********************************************************************************************************
Purpose:				set the size of the symbol table to 0
Author:					Thom Palmer
History/Versions:		10.18.13
Called functions:		none
Parameters:				void
Return value:			void
Algorithm:				Set the size of the symbol table to 0
**********************************************************************************************************/
static void st_setsize(void)
{
	sym_table.st_size = 0;
}
/**********************************************************************************************************
Purpose:				Increment the offset of the golbal symbol table. 
Author:					Chris Whitten
History/Versions:		10.18.13
Called functions:		none
Parameters:				void
Return value:			void
Algorithm:				Increment the global symbol table. 
**********************************************************************************************************/
static void st_incoffset(void)
{
	++sym_table.st_offset;
}
/**********************************************************************************************************
Purpose:				Store the elements of the symbol table into the file $stable.ste if the symbold table
						ever fills up causing the program to crash. 
Author:					Chris Whitten
History/Versions:		10.18.13
Called functions:		chk_sym_tbl(), fprintf(), st_get_type(),fclose(),printf()
Parameters:				STD sym_table
Return value:			int representing the number of records stored in the file on success -1 on failure
Algorithm:				Open the file, iterate over every element in the symbol table saving  their
						attributes into the file, then get the type of the element and print out it's 
						initial value correctly.
**********************************************************************************************************/
int st_store(STD sym_table)
{
	int i;		/* to iterate over the symbol tables elemens. */
	FILE *fi;	/* file pointer to work with. */
	chk_sym_tbl(sym_table);
	fi = fopen("$stable.ste", "w+"); /* Will throw warning. */
	if(fi)
	{
		fprintf(fi,"%d", sym_table.st_size);
		/* Iterate over the entire symbol table */
		for(i=0; i<sym_table.st_offset; i++)
		{
			fprintf(fi, " %4X %d %s %d", sym_table.pstvr[i].status_field, strlen(sym_table.pstvr[i].plex), sym_table.pstvr[i].plex, sym_table.pstvr[i].o_line);
			/* Get the type of the in order to print out the value correctly.  */
			switch(st_get_type(sym_table,i))
			{
			case FLT:
				fprintf(fi," %.2f", sym_table.pstvr[i].i_value.fpl_val);
				break;
			case INT:
				fprintf(fi," %d", sym_table.pstvr[i].i_value.int_val);
				break;
			case STR:
				fprintf(fi," %d", sym_table.pstvr[i].i_value.str_offset);
				break;
			}
		}
		/* Close the file and diplay to the user that the table ha sbeen stored successfully before exiting the function. */
		fclose(fi);
		printf("\nSymbol Table stored.\n");
		return i;	
	}	
	return FILE_NT_FND;
}
/**********************************************************************************************************
Purpose:				Find the column in the transition table that corresponds to the current character
Author:					Thom Palmer
History/Versions:		10.18.13
Called functions:		chk_sym_tbl(), qsort(), malloc(), st_compare_A(), b_getsize(), strlen(), b_reset(),
						b_addc, free(), b_get_chmemloc()
Parameters:				STD sym_table, char s_order
Return value:			1 on success, 0 on failure
Algorithm:				Check that the symbol table is valid, if s_order is A or D call qsort with the 
						corresponding function call, allocated memory for the temporary char *, iterate
						over each STVR and store the contents of their plexs in the char *, use b_reset
						to prepare the buffer for its new contents, store the contents of the char * into
						the buffer, free the char *, correct the plex pointers and return.
**********************************************************************************************************/
int st_sort(STD sym_table, char s_order)
{
	unsigned int i;											/* Used to iterate over STVR array */
	unsigned int j;											/* Used to iterate over characters in each plex */
	short initOffset = 0;									/* Stores the offset from the beginning of the buffer */
	int charOffset = 0;										/* Counts the offset from the beginning of the char* */
	char * orderedBufferCont = NULL;						/* Used to store the ordered lexemes from the buffer temporarily */
	unsigned short buffSize = b_getsize(sym_table.plsBD);	/* Needed to store the size of the buffer */	

	chk_sym_tbl(sym_table);
	
	switch(s_order)
	{
		/* Call qsort to sort the STVR array in ascending order */
		case 'A':
			qsort((void*)sym_table.pstvr, sym_table.st_offset,sizeof(STVR), st_compare_A);
			break;
		/* Call qsort to sort the STVR array in descending order */
		case 'D':
			qsort((void*)sym_table.pstvr, sym_table.st_offset,sizeof(STVR), st_compare_D);
			break;
		/* Invalid s_order return -1 */
		default:
			return INVLD_TYPE;
	}
	/* Allocate memory for a char* the same size as the buffsize */
	orderedBufferCont = (char*)malloc(sizeof(char)*buffSize);
	
	/* Iterate over the array of STVRs */
	/* Cast to unsigned because the offset is and index value which cannot be negative*/
	for(i = 0; i<(unsigned)sym_table.st_offset;i++)
	{	
		/* For each STVR.plex store its contents character by character into the temporary char* */
		for (j = 0;j<=strlen(sym_table.pstvr[i].plex);j++)
		{
			orderedBufferCont[charOffset] = sym_table.pstvr[i].plex[j];
			/* Increment charOffset each time */
			++charOffset;
		}	
	}
	/* Reset the buffer in preparation for adding the contents of the temporary char* to it */
	b_reset(sym_table.plsBD);

	/* Iterate over the buffer and add the ordered contents in */
	for( i = 0; i<buffSize;++i)
	{
		b_addc(sym_table.plsBD, orderedBufferCont[i]);	
	}
	/* Free the temprary char *. It is no longer neeeded */
	free((char*)orderedBufferCont);
	/* Cast to unsigned because the offset is and index value which cannot be negative*/
	/* Iterate over the STVR array and ensure that each plex is point to the correct memory location */
	for(i=0; i<(unsigned)sym_table.st_offset; i++)
	{			
		sym_table.pstvr[i].plex  = b_get_chmemloc(sym_table.plsBD, initOffset);
		/* Casting to a short because the plex is stored in the buffer which could never be larget that SHRT_MAX*/
		initOffset += (short)strlen(sym_table.pstvr[i].plex)+1;	
	}	
	return SRT_SUCCESS;
}
/**********************************************************************************************************
Purpose:				Compare two STVR's within the qsort function in order to sort them alphebetically 
						by their plex element. 
Author:					Thom Palmer
History/Versions:		10.18.13
Called functions:		strcmp()
Parameters:				STD sym_table
Return value:			int 
Algorithm:				Return the value of strcmp from the plex's on the two given elements. 
**********************************************************************************************************/
int st_compare_A(const void * pstvrA, const void * pstvrB)
{
	return strcmp(((STVR*)pstvrA)->plex,((STVR*)pstvrB)->plex);
}
/**********************************************************************************************************
Purpose:				Compare two STVR's within the qsort function in order to sort them in reverse 
						alphebetically by their plex element. 
Author:					Thom Palmer
History/Versions:		10.18.13
Called functions:		strcmp()
Parameters:				STD sym_table
Return value:			int 
Algorithm:				Return the inverse value of strcmp from the plex's on the two given elements. 
**********************************************************************************************************/
int st_compare_D(const void * pstvrA, const void * pstvrB)
{
	return -strcmp(((STVR*)pstvrA)->plex,((STVR*)pstvrB)->plex);
}
/**********************************************************************************************************
Purpose:				Test the st_update_type funtion. 
Author:					Chris Whitten
History/Versions:		10.18.13
Called functions:		st_get_type(), st_update_type(), st_store()
Parameters:				STD sym_table
Return value:			None
Algorithm:				Iterate of the symbol table getting each elements type and trying to change the type,
						then call the store function inroder to print out the status_filed in hex. 
**********************************************************************************************************/
void test_update_type(STD sym_table)
{
	int i; /* Used as an iterator*/
	if(! sym_table.st_size)
	{
		return;
	}
	for(i = 0; i<sym_table.st_offset; i++)
	{
		switch(st_get_type(sym_table,i))
		{
		case FLT:
			st_update_type(sym_table,i,STR);
			st_update_type(sym_table,i,INT);
			break;
		case INT:
			st_update_type(sym_table,i,FLT);
			break;
		case STR:
			st_update_type(sym_table, i, INT);
			break;
		}
	}
	st_store(sym_table);
}
	

