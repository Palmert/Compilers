#include "stable.h"
#include <string.h>

static void st_incoffset(void);
STD st_create(int st_size)
{
    STD localSTD;				/* Local variable for Symbol Table Descriptor*/

	/* Set offset to 0*/
	localSTD.st_offset = 0;	
	/* Allocate dynamic memory for an array of STVRs */
	localSTD.pstvr = (STVR*)malloc(sizeof(STVR)*st_size); // Possible incorrect usage of malloc
	/* Create a new buffer in additive mode */
	localSTD.plsBD = b_create(1000,15,'a'); // Examine initial size of buffer and increment
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
int st_install(STD sym_table, char *lexeme, int line)
{
	int dataType = FLT_TYPE;				/* Stores the datatype of the lexeme. Initialize as a Floating point data type. */
	int vid_offset;					/* Stores the offset where lexeme is found */
	int i;							/* Used and an iterator*/

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
	sym_table.pstvr[sym_table.st_offset].status_field |= SETDFLT; //Possibly wrong
	
	/* Iterate through the lexeme and store it as a c-type string within the symbol table buffer */ 
	for( i=0;i<=strlen(lexeme) + 1;i++)
	{
		/*  According to language specs the default type of an VID is Integer if it's first character is i, o, d, or n */
		if( i == 0 && (lexeme[i] == 'i' || lexeme[i] == 'd' || lexeme[i] == 'n' ||lexeme[i] == 'o'))
		{
			dataType = INT_TYPE; // Need to make preprocessors
		}
		/*  According to language specs the VID is a string if its last character is a # */
		if( i == strlen(lexeme) && (lexeme[i] == '#'));
		{
			dataType = STR_TYPE; // Need to make preprocessors
		}
		/* Store each character including the string terminator */ 
		b_addc(sym_table.plsBD, lexeme[i]);
	}

	switch(dataType)
	{
		case FLT_TYPE:
			/* Set the data type field to a float using a bitmask and set i_value to 0 */
			sym_table.pstvr[sym_table.st_offset].status_field &= SET12_10; //Possibly wrong
			sym_table.pstvr[sym_table.st_offset].i_value.fpl_val = 0.0f;
			break;
		case INT_TYPE:
			/* Set the data type field to an int using a bitmask and set i_value to 0 */
			sym_table.pstvr[sym_table.st_offset].status_field &= SET12_01; //Possibly wrong
			sym_table.pstvr[sym_table.st_offset].i_value.int_val = 0;
			break;
		case STR_TYPE:
			/* Set the data type field to a string using a bitmask and set i_value to -1 */
			sym_table.pstvr[sym_table.st_offset].status_field &= SET012_111; //Possibly wrong
			sym_table.pstvr[sym_table.st_offset].i_value.str_offset = -1;
			break;
	}
	/* Increment the current st_offset and return st_offset */
	st_incoffset();
	return sym_table.st_offset;
}
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

int st_update_type(STD sym_table,int vid_offset,char v_type)
{
	CHK_SYM_TBL(sym_table);
	if((sym_table.pstvr[vid_offset].status_field & CHK_LSB) == 1)
	{
		return PRV_UPDTD;
	}  

	sym_table.pstvr[vid_offset].status_field|= RESET12;

	switch(v_type)
	{
		case 'F':
			sym_table.pstvr[vid_offset].status_field|= SET12_10;
			break;
		case 'I':
			sym_table.pstvr[vid_offset].status_field|= SET12_01;
			break;
	}

	sym_table.pstvr[vid_offset].status_field |= SET_LSB;

	return vid_offset;	
}

int st_update_value(STD sym_table, int vid_offset,InitialValue i_value)
{
	//TODO return value on failure must be -1 
	CHK_SYM_TBL(sym_table);

	if(vid_offset > sym_table.st_size)
	{
		return -1; //Define preprocessor
	}

	sym_table.pstvr[vid_offset].i_value = i_value;
	return vid_offset;

}
char st_get_type (STD sym_table, int vid_offset)
{
	//TODO return value on failure must be -1 
	CHK_SYM_TBL(sym_table);

	if(vid_offset > sym_table.st_size)
	{
		return -1; //Define preprocessor
	}

	if((sym_table.pstvr[vid_offset].status_field & CHK_TYP )== 10)
	{
		return FLT;
	}

	if((sym_table.pstvr[vid_offset].status_field & CHK_TYP )== 01)
	{
		return INT;
	}

	if((sym_table.pstvr[vid_offset].status_field & CHK_TYP )== 11)
	{
		return STR;
	}
	// use define
	return -1;
}
void st_destroy(STD sym_table)
{

	b_destroy(sym_table.plsBD);
	free((STVR*)sym_table.pstvr);
}
int st_print(STD sym_table)
{
	int i; /* used as a counter to iterate over the symbol table*/
	// Must also return -1 on any failure. 
	CHK_SYM_TBL(sym_table);

	printf("\nSymbol Table\n");
	printf("____________\n");
	printf("\nLine Number Variable Identifier\n");
	for(i=0;i<sym_table.st_offset; i++)
	{
		if(sym_table.pstvr[i].plex != NULL)
		{
			printf("%2d%10s%s \n", sym_table.pstvr[i].o_line, "", sym_table.pstvr[i].plex);
		}
	}
	return i;
}
static void st_setsize(void)
{
	extern STD sym_table;
	sym_table.st_size = 0;
}
static void st_incoffset(void)
{
	extern STD sym_table;
	++sym_table.st_offset;
}
int st_store(STD sym_table)
{
	CHK_SYM_TBL(sym_table);
}
int st_sort(STD sym_table, char s_order)
{
	CHK_SYM_TBL(sym_table);
}
