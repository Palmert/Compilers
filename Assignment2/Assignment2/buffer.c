/*********************************************************************************************************
File name: buffer.c
Compiler: MS Visual Studio 2012
Author: Thom Palmer, 040 713 234 
Course: CST 8152 – Compilers, Lab Section: 401
Assignment: Assignment 1 
Date: Sept. 25th 2013
Professor: Sv. Ranev
Purpose: Definition of all functions used within the buffer 
Function list: b_create(), b_addc(), b_reset(), b_destroy(), b_isfull(), b_getsize(), b_getcapacity(),
				b_setmark(), b_getmark(), b_getmode(), b_load(), b_isempty(), b_eob(), b_getc(), b_print(),
				b_pack(), b_get_r_flag(), b_isempty(), b_retract(), b_get_getc_offset(), b_set_getc_offset(),
				b_get_chmemloc()
*********************************************************************************************************/
#include "buffer.h"

/**********************************************************************************************************
Purpose: Attempts to dynamically allocate memory for both a Buffer struct and a character buffer
Author: Thom Palmer
History/Versions: 1.0 
Called functions: calloc(), malloc(), free()
Parameters: short init_capacity,char inc_factor,char o_mode 
Return value: Buffer * newbuffer on success, NULL on failure
Algorithm: Validate parameters, allocate memory for Buffer struct and character array,and initialize buffer
			values based on passed parameters.
**********************************************************************************************************/

Buffer * b_create(short init_capacity,char inc_factor,char o_mode){
	Buffer * newbuffer = NULL; /*Temporary Buffer* for memory allocation*/
	switch(o_mode)
	{ 
		case 'f':
		case 'a':
		case 'm':
			break;
		default:
			return NULL;
	}
		
	/*Check that the inc_factor is invalid if o_mode is 'm'*/
	if (o_mode == 'm' && (inc_factor < ZERO || inc_factor > MAX_MULT ))
	{
		return NULL;
	}

	/*Check for valid parameters prior to memory allocation*/
	/*Parameters cannot be less than one or allocation will fail*/
	if(init_capacity < ONE )
	{
		return NULL;
	}
	/*Change mode to f if inc_factor is 0*/
	if (inc_factor == ZERO)
	{
		o_mode = 'f';
	}
	/*Allocate memory for one newBuffer* using calloc*/
	newbuffer = (Buffer*)calloc( ONE, sizeof(Buffer));
	/*Check to ensure that calloc succeeded*/
	if(newbuffer == NULL)
	{
		return NULL;
	}
	/*Allocate memory for the char*buffer using malloc*/
	newbuffer->ca_head = (char*)malloc(init_capacity*sizeof(char));
	/*Check to ensure that malloc succeeded*/
	if(newbuffer->ca_head == NULL)
	{
		free((Buffer*)newbuffer);
		return NULL; 
	}	
	/*Initialize newbuffer mode and inc_factor based on o_mode*/
	switch (o_mode)
	{
		/*Fixed mode*/
		case 'f': 
			newbuffer->mode = FIXED;
			newbuffer->inc_factor = ZERO;				 
			break;
		/*Additive mode*/
		case 'a':			
			newbuffer->mode = ADDITIVE;
			newbuffer->inc_factor = (unsigned char)inc_factor;			
			break;
		/*Multiplicative mode*/
		case 'm': 			
			newbuffer->mode = MULTIPLICATIVE;
			newbuffer->inc_factor = inc_factor;
			break;
	}
	/*Initialize newbuffer capacity to init_capacity*/
	newbuffer->capacity=init_capacity;
	return newbuffer;
}

/**********************************************************************************************************
Purpose: Adds characters to the character buffer and increases the buffer size if it is full
Author: Thom Palmer
History/Versions: 1.0 
Called functions: realloc()
Parameters: Buffer * const pBD, char symbol[for each formal parameter: type, specific range or values if applicable] 
Return value: Buffer * pBD on success, NULL on failure
Algorithm: Declare temporary variables, validate parameters, check if character buffer is full, if not add
			symbol to buffer at offset location, else determine operational mode and increase capacity using
			realloc accordingly, check if location of character buffer has changed and set r_flag if it is,
			add symbol to buffer at offset
			*It is assumed that capacity and addc_offset are the same size*
**********************************************************************************************************/
Buffer * b_addc(Buffer * const pBD, char symbol)
{
	short nCapacity = ZERO;			/*Used to store and calculate new capacity*/
	float spaceAvail = ZERO;		/*Stores how much space is available between SHRT_MAX and the current capacity*/
	short increment = ZERO;			/*Used to store and calculate new increment*/
	char * newHead = NULL;			/*Used to safely realloc pBD->ca_head and compare change in address*/
	
	/*Check for invalid parameters*/
	if(pBD == NULL )
	{
		return NULL;
	}
	if(b_isfull(pBD) && pBD->capacity == SHRT_MAX)
	{ 
		return NULL;
	}
	pBD->r_flag = ZERO;	

	/*Check if char buffer has room to add symbol*/
	if(pBD->capacity > pBD->addc_offset)
	{
		/*Add symbol to char buffer at current offset*/
		pBD->ca_head[pBD->addc_offset] = symbol;
		pBD->addc_offset++;
		return pBD;
	}	
	/*Handle capacity increase based on operational mode*/
	switch (pBD->mode)
	{
		/*Fixed mode never increases capacity*/
		case FIXED:	
			return NULL;
			break;
		/*Additive mode increases capacity by inc_factor*/
		case ADDITIVE: 
			nCapacity = pBD->capacity + (unsigned char)pBD->inc_factor;
			/*Check for overflow*/
			if(nCapacity < ZERO)
			{
				return NULL;
			}
			break;
		case MULTIPLICATIVE: 
			/*Calculate amount of space available to increase buffer*/
			spaceAvail = (float)(SHRT_MAX - pBD->capacity);
			/*Calculate new increment*/
			increment  = (short)(spaceAvail * pBD->inc_factor / ONEHUNDRED);
			/*Calculate new capacity and test for validity*/
			nCapacity  = pBD->capacity + increment;
			/*Assign pBD->capacity to max buffer size if nCapacity was unable to increase*/
			if(pBD->capacity == nCapacity && nCapacity < SHRT_MAX) 
			{
				nCapacity = SHRT_MAX;
			}

			break;
	}	
	/*realloc char*buffer based on new capacity and check for success*/
	newHead = (char*)realloc((char*)pBD->ca_head, nCapacity);
	if(newHead == NULL)
	{
		return NULL;
	}
	/*Check if address location has changed. Set ca_head to new address and set r_flag*/
	else if(pBD->ca_head != newHead)
	{
		pBD->ca_head = newHead;
		pBD->r_flag = SET_R_FLAG;		
	}
	/* Set the new capacity of the buffer after every possible fail case to ensure the buffer descriptor is accurate*/	 
	pBD->capacity = nCapacity;

	/*Add symbol to char*buffer at offset location*/
	pBD->ca_head[pBD->addc_offset] = symbol;	
	pBD->addc_offset++;
	return pBD;
}

/**********************************************************************************************************
Purpose: Reset addc_offset to ZERO to allow buffer to add characters to beginning to the buffer
Author: Thom Palmer
History/Versions: 1.0 
Called functions: none
Parameters: Buffer * const pBD
Return value: int ONE on sucess, int R_FAIL_1 on failure
Algorithm:
**********************************************************************************************************/
int b_reset(Buffer * const pBD)
{
	if(pBD==NULL)
	{
		return R_FAIL_1;
	}
	pBD->mark_offset = ZERO;
	pBD->addc_offset = ZERO;
	pBD->getc_offset = ZERO;
	pBD->eob		 = ZERO;
	pBD->r_flag		 = ZERO;
	return ONE;
}
/**********************************************************************************************************
Purpose: Free all dynamically allocated memory
Author: Thom Palmer
History/Versions: 1.0 
Called functions: free()
Parameters: Buffer * const pBD 
Return value: none
Algorithm: 
**********************************************************************************************************/
void b_destroy(Buffer * const pBD)
{
	if(pBD==NULL)
	{
		return;
	}
	free(pBD->ca_head);
	free((Buffer*)pBD);
}
/**********************************************************************************************************
Purpose: Check if character buffer is full
Author: Thom Palmer
History/Versions: 1.0 
Called functions: none
Parameters: Buffer * const pBD
Return value: int ONE if full, int ZERO if not full, int R_FAIL_1 on failure
Algorithm: *Assume that pBD->capacity and pBD->addc_offset are the same measurement*
**********************************************************************************************************/
#ifndef B_FULL
int b_isfull(Buffer * const pBD) 
{
	if(pBD==NULL)
	{
		return R_FAIL_1;
	}
	/*Check if character buffer is full*/
	if(pBD->capacity - pBD->addc_offset == ZERO)
	{
		return ONE;
	}
	return ZERO;
}
#endif
/**********************************************************************************************************
Purpose: Returns size used in character buffer
Author: Thom Palmer
History/Versions: 1.0 
Called functions: none
Parameters: Buffer * const pBD
Return value: shortpBD->addc_offset RANGE: ZERO-SHRTMAX
Algorithm: 
**********************************************************************************************************/
short b_getsize(Buffer * const pBD)
{
	if(pBD==NULL)
	{
		return R_FAIL_1;
	}
	return pBD->addc_offset;
}
/*********************************************************************************************************
Purpose: Returns current capacity of the character buffer
Author: Thom Palmer
History/Versions: 1.0 
Called functions: none
Parameters: Buffer * const pBD
Return value: short pBD->capacity on success, short R_FAIL_1 on failure
Algorithm: 
**********************************************************************************************************/
short b_getcapacity(Buffer * const pBD)
{
	if(pBD==NULL)
	{
		return R_FAIL_1;
	}
	return pBD->capacity;
}
/**********************************************************************************************************
Purpose: Stores a mark to the end of the current input in mark_offset
Author: Thom Palmer
History/Versions: 1.0 
Called functions: none
Parameters: Buffer * const pBD, short mark
Return value: int ONE on success, int R_FAIL_1 on failure
Algorithm: *Assume mark and pBD->capacity are the same measurements*
**********************************************************************************************************/
int b_setmark(Buffer * const pBD, short mark)
{
	if(pBD==NULL)
	{
		return R_FAIL_1;
	}
	if(mark<pBD->capacity && mark>=0)
	{
		pBD->mark_offset = mark;
		return ONE;
	}
	return R_FAIL_1;
}
/**********************************************************************************************************
Purpose: Return mark_offset to the calling function
Author: Thom Palmer
History/Versions: 1.0 
Called functions: none
Parameters: Buffer * const pBD 
Return value: short pBD->mark_offset on success, short R_FAIL_1 on failure
Algorithm: 
**********************************************************************************************************/
short b_getmark(Buffer * const pBD)
{
	if(pBD==NULL)
	{
		return R_FAIL_1;
	}
	return pBD->mark_offset;
}
/**********************************************************************************************************
Purpose: Returns current mode to the calling function
Author: Thom Palmer
History/Versions: 1.0 
Called functions: none
Parameters: Buffer * const pBD 
Return value: int pBD->mode on success, int R_FAIL_2 on failure
Algorithm: 
**********************************************************************************************************/
int b_getmode(Buffer * const pBD)
{
	if(pBD==NULL)
	{
		return R_FAIL_2;
	}
	return pBD->mode;
}
/**********************************************************************************************************
Purpose: Gets characters from the file until EOF is reached
Author: Thom Palmer
History/Versions: 1.0 
Called functions: feof(),fgetc() and b_addc()
Parameters: FILE *const fi, Buffer * const pBD
Return value: int charsAdded on success, int R_FAIL_1 and int LOAD_FAIL on failure
Algorithm: Get a character from fi while character is not EOF, call b_addc and pass pBD and current
				charToAdd return number of characters added to buffer
**********************************************************************************************************/
int b_load(FILE *const fi, Buffer * const pBD)
{
	char charToAdd;			/*Current character to add to the buffer*/
	int charsAdded = ZERO;	/*Number of characters added*/

	if(fi==NULL)
	{
		return R_FAIL_1;
	}
	if(pBD==NULL)
	{
		return R_FAIL_1;
	}

	/*Get characters from the fi while end of file has not been reached*/
	/*Using a for loop negates the need for a secondary check on feof(fi)*/
	for(charToAdd = (char)fgetc(fi); !feof(fi); charToAdd = (char)fgetc(fi))
	{	
		/*Pass pBD and the charToADD into b_addc and check for a failure in b_addc*/
		if(!b_addc(pBD,charToAdd))
		{
			return LOAD_FAIL;
		}
		charsAdded++;
	}
	return charsAdded;
}	
/**********************************************************************************************************
Purpose: Check if character buffer is empty
Author: Thom Palmer
History/Versions: 1.0 
Called functions: none
Parameters: Buffer * const pBD
Return value: int ONE if empty, int ZERO if not, int R_FAIL_1 on failure
Algorithm: 
**********************************************************************************************************/
int b_isempty(Buffer * const pBD)
{
	if(pBD == NULL)
	{
		return R_FAIL_1;
	}
	if(pBD->addc_offset == ZERO)
	{
		return ONE;
	}
	return ZERO;
}
/**********************************************************************************************************
Purpose: Return eob flag to calling function
Author: Thom Palmer
History/Versions: 1.0 
Called functions: none
Parameters: Buffer * const pBD
Return value: int pBD->eob on success, int R_FAIL_1 on failure
Algorithm: 
**********************************************************************************************************/
int b_eob(Buffer * const pBD)
{
	if(pBD == NULL)
	{
		return R_FAIL_1;
	}
	return pBD->eob;
}
/**********************************************************************************************************
Purpose: Returns the char in the character buffer at getc_offset to the calling function
Author: Thom Palmer
History/Versions: 1.0 
Called functions: none
Parameters: Buffer * const pBD 
Return value: char pBD->ca_head[pBD->getc_offset] on success, char R_FAIL_1 if eob has been reach and 
				char R_FAIL_2 on failure
Algorithm: [outline the main steps (sections) only; do not include implementation 
details; for small and clear functions leave this empty]
**********************************************************************************************************/
char b_getc(Buffer * const pBD)
{
	/*Check parameters for validity*/
	if(pBD == NULL) 
	{
		return R_FAIL_2;
	}
	/*Check if end of character buffer has been reached and set eob flag*/
	if(pBD->addc_offset == pBD->getc_offset)
	{
		pBD->eob = ONE;		
		return R_FAIL_1;
	}
	pBD->eob = ZERO;
	/*Return character and increment getc_offset*/
	return pBD->ca_head[pBD->getc_offset++];
}

/**********************************************************************************************************
Purpose: Prints the contents of the character buffer
Author: Thom Palmer
History/Versions: 1.0 
Called functions: b_isempty(), printf(), b_set_getc_offset(), b_getc(), b_eob()
Parameters: Buffer * const pBD
Return value: int charsPrinted on success, int R_FAIL_1 on failure
Algorithm: Check for invalid parameters, check if the buffer is empty, call b_set_getc_offset,
			print out characters within the buffer using b_getc and printf while the eob has not
			been reached, increment the number of charsPrinted each time the loop iterates, 
			return the number of charsPrinted
**********************************************************************************************************/
int b_print(Buffer * const pBD)
{	
	char c;						/*Character to print*/
	int charsPrinted = ZERO ;	/*Number of characters printed*/

	/*Check for invalid parameters*/
	if(pBD == NULL)
	{
		return R_FAIL_1;
	}

	/*Call b_isempty and print if the buffer is empty*/
	if(b_isempty(pBD))
	{
		printf("The buffer is empty.\n");
		return charsPrinted;
	}
	/*Call b_set_getc_offset*/
	b_set_getc_offset(pBD, ZERO);
	
	/*Use a do{}While; loops ensures that b_getc will be called and eob will be reset appropriately*/
	/*Get a character from the buffer, print it, and increment charsPrinted while !b_eob*/
	do 
	{
		c = b_getc(pBD);
		/*Check explicitly if b_getc failed or it will print incorrect characters*/
		if ( c == R_FAIL_2)
		{
			break;
		}
		/*R_FAIL_1 also prints out EOF. MUST check b_eob to ensure it is the the EOF char from the buffer*/
		if ( c == R_FAIL_1 && b_eob(pBD))
		{
			break;
		}
		printf("%c", c);
		charsPrinted++;
	}while(!b_eob(pBD));
	
	printf("\n");
	return charsPrinted;
}	
/**********************************************************************************************************
Purpose: Resizes the character buffer to the size + 1 of the characters added to it
Author: Thom Palmer
History/Versions: 1.0 
Called functions: realloc()
Parameters: Buffer * const pBD
Return value: Buffer *pBD on success, NULL on failure
Algorithm: Validate parameters, set the capacity to addc_offest + 1, resize char buffer using realloc,
			return Buffer * pBD
			*Assume that pBD->capacity and pBD->addc_offset are the same measurements*
**********************************************************************************************************/
Buffer *b_pack(Buffer * const pBD)
{
	char *newHead = NULL;		/*Used to safely realloc pBD->ca_head and compare change in address*/

	/*Check for invalid parameters*/
	if(pBD == NULL)
	{
		return NULL;
	}
	if (pBD->addc_offset == SHRT_MAX)
	{
		return NULL;
	}
	/*Reset r_flag*/
	pBD->r_flag = ZERO;
	/*Set new capacity and realloc*/
	pBD->capacity = pBD->addc_offset + ONE;
	if(pBD->capacity <= ZERO)
	{
		return NULL;
	}

	newHead = (char*)realloc((char*)pBD->ca_head,pBD->capacity);
	if(newHead == NULL)
	{
		return NULL;
	}
	/*Check if address location has changed. Set ca_head to new address and set r_flag*/
	else if(pBD->ca_head != newHead)
	{
		pBD->ca_head = newHead;
		pBD->r_flag = SET_R_FLAG;		
	}
	return pBD;
}
/**********************************************************************************************************
Purpose: Returns r_flag to the calling function
Author: Thom Palmer
History/Versions: 1.0 
Called functions: none
Parameters: Buffer * const pBD
Return value: char pBD->r_flag on success, char R_FAIL_1 on failure 
Algorithm: 
**********************************************************************************************************/
char b_get_r_flag(Buffer * const pBD)
{
	if(pBD == NULL)
	{
		return R_FAIL_1;
	}
	return pBD->r_flag;
}
/**********************************************************************************************************
Purpose: Decrease the getc_offset by 1
Author: Thom Palmer
History/Versions: 1.0 
Called functions: none
Parameters: Buffer * const pBD
Return value: int ONE on success, int R_FAIL_1 on failure
Algorithm: 
**********************************************************************************************************/
int b_retract(Buffer * const pBD)
{
	if(pBD == NULL)
	{
		return R_FAIL_1;
	}	
	if(pBD->getc_offset == ZERO)
	{
		return R_FAIL_1;
	}
	--pBD->getc_offset;
	return ONE;
}
/**********************************************************************************************************
Purpose: Return getc_offset to the calling function
Author: Thom Palmer
History/Versions: 1.0 
Called functions: none
Parameters: Buffer * const pBD
Return value: short pBD->getc_offset, short R_FAIL_1 on failure 
Algorithm:
**********************************************************************************************************/
short b_get_getc_offset(Buffer * const pBD)
{
	if(pBD == NULL)
	{
		return R_FAIL_1;
	}
	return pBD->getc_offset;
}
/**********************************************************************************************************
Purpose: Set getc_offset to offset
Author: Thom Palmer
History/Versions: 1.0 
Called functions: none
Parameters: Buffer * const pBD, short offset
Return value: int pBD->getc_offset on success, int R_FAIL_1 on failure
Algorithm: *Assume that offset and pBD->capacity are the same measurements*
**********************************************************************************************************/
int b_set_getc_offset(Buffer * const pBD, short offset)
{
	if(pBD == NULL)
	{
		return R_FAIL_1;
	}
	if(offset > pBD->capacity || offset < ZERO)
	{
		return R_FAIL_1;
	}
	pBD->getc_offset = offset;
	return pBD->getc_offset;
}
/**********************************************************************************************************
Purpose: Return the address to a specific character within the character buffer
Author: Thom Palmer
History/Versions: 1.0 
Called functions: none
Parameters: Buffer * const pBD, short offset 
Return value: char * ptr on success, NULL on failure
Algorithm: *Assume that offset and pBD->capacity are the same measurements*
**********************************************************************************************************/
char * b_get_chmemloc(Buffer * const pBD, short offset)
{
	char * ptr = NULL;			/*Holds address to requested character in character buffer*/

	if(pBD == NULL)
	{
		return NULL;
	}
	/*Check that offset will not throw an out of bounds exception*/
	if(offset < ZERO || offset >= pBD->capacity)
	{ 
		return NULL;
	}
	ptr = &pBD->ca_head[offset];
	return ptr;
}


