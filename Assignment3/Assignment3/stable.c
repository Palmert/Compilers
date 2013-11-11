#include "stable.h"

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
	CHK_SYM_TBL
	if(st_lookup(sym_table, lexeme) != -1)
	{
		
	}
	
	sym_table.pstvr->plex = b_get_chmemloc(sym_table.plsBD, b_getsize(sym_table.plsBD));


}
int st_lookup(STD sym_table,char *lexeme)
{
	CHK_SYM_TBL

}

int st_update_type(STD sym_table,int vid_offset,char v_type)
{
	CHK_SYM_TBL
	if((sym_table.pstvr->status_field & CHK_LSB) == 1)
	{
		return PRV_UPDTD;
	}  

	sym_table.pstvr->status_field|= RESET12;
	switch(v_type)
	{
	case 'F':
		sym_table.pstvr->status_field|= SET12_10;
		break;
	case 'I':
		sym_table.pstvr->status_field|= SET12_01;
		break;

	}

	sym_table.pstvr->status_field |= SET_LSB;
	
}
int st_update_value(STD sym_table, int vid_offset,InitialValue i_value)
{
	CHK_SYM_TBL

}
char st_get_type (STD sym_table, int vid_offset)
{
	CHK_SYM_TBL
}
void st_destroy(STD sym_table)
{
	CHK_SYM_TBL
}
int st_print(STD sym_table)
{
	CHK_SYM_TBL
}
static void st_setsize(void)
{
	
}
static void st_incoffset(void)
{
	CHK_SYM_TBL
}
int st_store(STD sym_table)
{
	CHK_SYM_TBL
}
int st_sort(STD sym_table, char s_order)
{
	CHK_SYM_TBL
}
