#include "buffer.h"

#ifndef _STBL_H
#define _STBL_H

#define INVLD_SYM_TBL 0
#define ERR_SYM_TBL -5
#define PRV_UPDTD -1


#ifndef CHK_SYM_TBL
#define CHK_SYM_TBL { if (sym_table.st_size == INVLD_SYM_TBL) return ERR_SYM_TBL; }
#endif

/*Bit masks*/
#define CHK_LSB     0x0001   /* 0000 0000 0000 0001 */
#define SET_LSB     0x0001   /* 0000 0000 0000 0001 */
#define RESET12		0xFFF9   /* 1111 1111 1111 1001 */
#define SET12_01    0x0002   /* 0000 0000 0000 0010 */
#define SET12_10    0x0004   /* 0000 0000 0000 0100 */

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
int st_update_value(STD sym_table, int vid_offset,
InitialValue i_value);
char st_get_type (STD sym_table, int vid_offset);
void st_destroy(STD sym_table);
int st_print(STD sym_table);
static void st_setsize(void);
static void st_incoffset(void);
int st_store(STD sym_table);
int st_sort(STD sym_table, char s_order);

#endif


