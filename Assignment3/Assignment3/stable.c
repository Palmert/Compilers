#include "stable.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

static void st_incoffset(void);
	

/**********************************************************************************************************
Purpose:				Create a new SymbolTableDescriptor.
Author:					Chris Whitten
History/Versions:		10.18.13
Called functions:		malloc(), sizeof(), b_create()
Parameters:				STD sym_table,char *lexeme
Return value:			Offset of the STVR if it already exists -1 on failure.  
Algorithm:				
**********************************************************************************************************/
STD st_create(int st_size)
{
    STD localSTD;				/* Local variable for Symbol Table Descriptor*/

	/* Set offset to 0*/
	localSTD.st_offset = 0;	
	/* Allocate dynamic memory for an array of STVRs */
	localSTD.pstvr = (STVR*)malloc(sizeof(STVR)*st_size); // Possible incorrect usage of malloc
	/* Create a new buffer in additive mode */
	localSTD.plsBD = b_create(10,10,'a'); // Examine initial size of buffer and increment
	/* Check for a failure in either of the malloc's*/
	if(!localSTD.pstvr || !localSTD.plsBD)
	{
		/* Set the size of the STD to 0 and return STD*/
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
Author:					Chris Whitten
History/Versions:		10.18.13
Called functions:		CHK_SYM_TBL(), st_lookup(), b_get_chmemloc(), b_getsize(), strlen(), b_addc(),
						b_get_r_flag()
Parameters:				STD sym_table,char *lexeme
Return value:			Offset of the STVR if it already exists -1 on failure.  
Algorithm:				
**********************************************************************************************************/
int st_install(STD sym_table, char *lexeme, int line)
{
	int dataType = FLT_TYPE;				/* Stores the datatype of the lexeme. Initialize as a Floating point data type. */
	int vid_offset;					/* Stores the offset where lexeme is found */
	int i;							/* Used and an iterator*/
	short offset = 0;
	int lexlen = 0;

	/*Check for valid symbol table*/
	CHK_SYM_TBL(sym_table);
	/* Check if symbol table is full */
	if(sym_table.st_size == sym_table.st_offset)
	{
		return -1;
	}
	/* Call st_lookup to search for the lexeme in the table */
	vid_offset = st_lookup(sym_table, lexeme);
	/* If the lexeme was found. Return the offset where it was found */
	if(vid_offset != -1)
	{
		return vid_offset;
	}	
	/* Set the plex point using a call to b_get_chmemloc. This is the only buffer function that returns a pointer */
	sym_table.pstvr[sym_table.st_offset].plex  = b_get_chmemloc(sym_table.plsBD, b_getsize(sym_table.plsBD));
	/* Set o_line to the current line number */
	sym_table.pstvr[sym_table.st_offset].o_line = line;
	/* Set the status field to default values using the default bitmask */
	sym_table.pstvr[sym_table.st_offset].status_field &= DEFAULTZ;
	sym_table.pstvr[sym_table.st_offset].status_field |= SETDFLT; //Possibly wrong
	
	/* Iterate through the lexeme and store it as a c-type string within the symbol table buffer */ 
	for( i=0;i<=strlen(lexeme) + 1;i++)
	{
		++lexlen;
		/*  According to language specs the default type of an VID is Integer if it's first character is i, o, d, or n */
		if( i == 0 && (lexeme[i] == 'i' || lexeme[i] == 'd' || lexeme[i] == 'n' ||lexeme[i] == 'o'))
		{
			dataType = INT_TYPE; // Need to make preprocessors
		}
		/*  According to language specs the VID is a string if its last character is a # */

		if( i == strlen(lexeme) -1  && lexeme[i] == '#')
		{
			dataType = STR_TYPE; // Need to make preprocessors
		}
		/* Store each character including the string terminator */ 
		b_addc(sym_table.plsBD, lexeme[i]);
		if(b_get_r_flag(sym_table.plsBD))
		{
			offset = 0;
			for(i=0; i<sym_table.st_offset; i++)
				{		
					
					sym_table.pstvr[i].plex  = b_get_chmemloc(sym_table.plsBD, offset);
					offset += strlen(sym_table.pstvr[i].plex)+1;
					
				}	
			sym_table.pstvr[sym_table.st_offset].plex = b_get_chmemloc(sym_table.plsBD, b_getsize(sym_table.plsBD) -lexlen);
		}
	}

	switch(dataType)
	{
		case FLT_TYPE:
			/* Set the data type field to a float using a bitmask and set i_value to 0 */
			sym_table.pstvr[sym_table.st_offset].status_field |= SET12_10; //Possibly wrong
			sym_table.pstvr[sym_table.st_offset].i_value.fpl_val = 0.0f;
			break;
		case INT_TYPE:
			/* Set the data type field to an int using a bitmask and set i_value to 0 */
			sym_table.pstvr[sym_table.st_offset].status_field |= SET12_01; //Possibly wrong
			sym_table.pstvr[sym_table.st_offset].i_value.int_val = 0;
			break;
		case STR_TYPE:
			/* Set the data type field to a string using a bitmask and set i_value to -1 */
			sym_table.pstvr[sym_table.st_offset].status_field |= SET012_111; //Possibly wrong
			sym_table.pstvr[sym_table.st_offset].i_value.str_offset = -1;
			break;
	}
	/* Increment the current st_offset and return st_offset */
	st_incoffset();
	return sym_table.st_offset;
}

/**********************************************************************************************************
Purpose:				Check if a STVR with the given lexeme already exists. 
Author:					Chris Whitten
History/Versions:		10.18.13
Called functions:		CHK_SYM_TBL(), strcmp()
Parameters:				STD sym_table,char *lexeme
Return value:			Offset of the STVR if it already exists -1 on failure.  
Algorithm:				Esnure a valid symbol table was given then iterate of each element comapring the 
						plex, if a match is found return the offset otherwise return -1.
**********************************************************************************************************/
int st_lookup(STD sym_table,char *lexeme)
{
	int i;							/* Used as an iterator*/

	CHK_SYM_TBL(sym_table);

	/* Iterate over every element in the symbol table */
	for(i=sym_table.st_offset -1; i >= 0; i--)
	{
		/* If the lexeme is found in the symbol table buffer return the index where it was found */

		if( strcmp(lexeme, sym_table.pstvr[i].plex) == 0)
		{
			return i;
		}
	}
	/* If the lexeme was not found return -1 to inform the calling function. */
	return LEX_NOT_FND;
}

/**********************************************************************************************************
Purpose:				Update the type of the SVTR at the given offset. 
Author:					Chris Whitten
History/Versions:		10.18.13
Called functions:		CHK_SYM_TBL()
Parameters:				STD sym_table, int vid_offset,InitialValue i_value
Return value:			Offset of the updated STVR. -1 on failure. 
Algorithm:				Check for a valid symbol table and that the offset is within the size of the symbol
						table. If the table and offset are valid check if the STVR has preiously been updated, 
						if it has not been update it and set the update flag then return. otherwise return 
						a failure. 
**********************************************************************************************************/
int st_update_type(STD sym_table,int vid_offset,char v_type)
{
	CHK_SYM_TBL(sym_table);

	/* Ensure the offset is within range of valid STVR's */
	if(vid_offset >= sym_table.st_offset) 
	{
		return INVLD_OFFSET; 
	}

	/* Check to make sure the type has not been presiousy updated. */
	if((sym_table.pstvr[vid_offset].status_field & CHK_LSB) == PRV_UPDTD)
	{
		return ERR_PRV_UPDTD;
	}  
	/* Rested the flags before updating the type of the STVR */
	sym_table.pstvr[vid_offset].status_field|= RESET12;

	/* depending on the type update the status filed accordingly.*/
	switch(v_type)
	{
		case FLT:
			sym_table.pstvr[vid_offset].status_field|= SET12_10;
			break;
		case INT:
			sym_table.pstvr[vid_offset].status_field|= SET12_01;
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
Called functions:		CHK_SYM_TBL()
Parameters:				STD sym_table, int vid_offset,InitialValue i_value
Return value:			Offset of the updated STVR. -1 on failure. 
Algorithm:				Check for a valid symbol table and that the offset is within the size of the symbol
						table, then update the value with the given value. 
**********************************************************************************************************/
int st_update_value(STD sym_table, int vid_offset,InitialValue i_value)
{
	//TODO return value on failure must be -1 
	CHK_SYM_TBL(sym_table);

	/* Ensure the offset is within range of valid STVR's */
	if(vid_offset >= sym_table.st_offset) 
	{
		return INVLD_OFFSET; 
	}

	sym_table.pstvr[vid_offset].i_value = i_value;
	return vid_offset;

}


/**********************************************************************************************************
Purpose:				Get the type of the STVR at the given offset. 
Author:					Chris Whitten
History/Versions:		10.18.13
Called functions:		CHK_SYM_TBL()
Parameters:				STD sym_table, int vid_offset
Return value:			char F if it's a float, I if it's an int, S if it's a string. -1 on failure. 
Algorithm:				Check for a valid symbol table, then compare the status field of the
						STVR at the given offset by using bit masking then return the corresponidng 
						char that represents the type. 
**********************************************************************************************************/
char st_get_type (STD sym_table, int vid_offset)
{
	//TODO return value on failure must be -1 
	CHK_SYM_TBL(sym_table);

	/* Ensure the offset is within range of valid STVR's */
	if(vid_offset >= sym_table.st_offset)
	{
		return INVLD_OFFSET; 
	}
	switch(sym_table.pstvr[vid_offset].status_field & CHK_TYP )
	{
	case INT_TYPE:
		return INT;

	case FLT_TYPE:
		return FLT;

	case STR_TYPE:
		return STR;
	}
	// Talk to thom
}


/**********************************************************************************************************
Purpose:				Free all the dynamically allocated memory associated with the symbol table. 
Author:					Chris Whitten
History/Versions:		10.18.13
Called functions:		b_destroy(), free()
Parameters:				STD sym_table
Return value:			void
Algorithm:				Check for a valid symbol table, then destroy the buffer and free the array of 
						STVRs
**********************************************************************************************************/
void st_destroy(STD sym_table)
{
	if(sym_table.st_size)
	{
		b_destroy(sym_table.plsBD);
		free((STVR*)sym_table.pstvr);
	}
}

/**********************************************************************************************************
Purpose:				Print the contents of the symbol table. 
Author:					Chris Whitten
History/Versions:		10.18.13
Called functions:		printf(), CHK_SYM_TBL()
Parameters:				STD sym_table
Return value:			number of elements printed on success -1 on failure.
Algorithm:				Check for a valid symbol table, then interate over every element in the table 
						printing out it's attributes. 
**********************************************************************************************************/
int st_print(STD sym_table)
{
	int i; /* used as a counter to iterate over the symbol table */
	// Must also return -1 on any failure. 
	CHK_SYM_TBL(sym_table);

	printf("\nSymbol Table\n");
	printf("____________\n");
	printf("\nLine Number Variable Identifier\n");
	/* iterate over entire symbole table printing the curren STVR's attributes */
	for(i=0;i<sym_table.st_offset; i++)
	{
		if(sym_table.pstvr[i].plex != NULL)
		{
			printf("%2d%10c%s\n", sym_table.pstvr[i].o_line, ' ', sym_table.pstvr[i].plex);
		}
	}
	return i;
}

/**********************************************************************************************************
Purpose:				set the size of the symbol table to 0
Author:					Chris Whitten
History/Versions:		10.18.13
Called functions:		none
Parameters:				void
Return value:			void
Algorithm:				set the size of the symbol table to 0
**********************************************************************************************************/
static void st_setsize(void)
{
	extern STD sym_table;
	sym_table.st_size = 0;
}


/**********************************************************************************************************
Purpose:				Increment the offset of the golbal symbol table. 
Author:					Chris Whitten
History/Versions:		10.18.13
Called functions:		none
Parameters:				void
Return value:			void
Algorithm:				increment the global symbol table. 
**********************************************************************************************************/
static void st_incoffset(void)
{
	extern STD sym_table;
	++sym_table.st_offset;
}


/**********************************************************************************************************
Purpose:				Find the column in the transition table that corresponds to the current character
Author:					Chris Whitten
History/Versions:		10.18.13
Called functions:		CHK_SYM_TBL(), fprintf(), st_get_type(),fclose(),printf()
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
	CHK_SYM_TBL(sym_table);
	fi = fopen("$stable.ste", "w+");
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
int st_sort(STD sym_table, char s_order)
{
	CHK_SYM_TBL(sym_table);
	if(s_order == 'A')
	{
		qsort(sym_table.pstvr, sym_table.st_offset,sizeof(STVR), st_compare_A);
		return 1;
	}
	qsort(sym_table.pstvr, sym_table.st_offset,sizeof(STVR), st_compare_D);
	return 1;
}

int st_compare_A(const void * pstvrA, const void * pstvrB)
{
	
	int i = 0;

	i = strcmp(((STVR*)pstvrA)->plex,((STVR*)pstvrB)->plex);
	if(i<0)
	{
		return -1;
	}
	if(i==0)
	{
		return 0;
	}
	return 1;	
}

int st_compare_D(const void * pstvrA, const void * pstvrB)
{
	int i = 0;

	i = strcmp(((STVR*)pstvrA)->plex,((STVR*)pstvrB)->plex);
	if(i<0)
	{
		return 1;
	}
	if(i==0)
	{
		return 0;
	}
	return -1;	
}