#include <stdlib.h>
#include <stdio.h>

#include "inc/rtlib.h"

#define PTR_TBL_SIZE (32768 * 8)

TBSTRING **temps = 0;
int temp_count = 0;

void markIfTemp(TBSTRING *s)
{
	if(s && strIsTemp(*s)) {
		strSetIsUsed(*s);
	}
}

void markIfResult(TBSTRING *s)
{
	if(s && strIsResult(*s) && s->old_temp) {
		strSetIsUsed(*(s->old_temp));
	}
}

void markIfLocal(TBSTRING *s)
{
	//if(s && strIsLocal(*s) && s->old_temp) {
	//	strSetIsUsed(*(s->old_temp));
	//}
}

TBSTRING *TBSTRING_alloc_temp(int len)
{
	TBSTRING *result = (TBSTRING *)calloc(sizeof(TBSTRING), 1);

	if(temps == 0) {
		temps = (TBSTRING **)calloc(PTR_TBL_SIZE * sizeof(TBSTRING *), 1);
	}

	if(temp_count < PTR_TBL_SIZE) {
		temps[temp_count] = result;
		temp_count += 1;
	} else {
		puts( "string table overflow!" );
		temp_count = 0;
	}

	if(len) {
		result->data = calloc(len + 1, 1);
		result->len = len;
		result->size = len + 1;
	}

	strSetIsTemp(*result);

	return result;
}

void free_strings_final()
{
	int i;

	//printf( "temp_count : %i\n", temp_count );
	for(i = 0; i < temp_count; i++) {
		//if(temps[i]->data) {
		//	printf("Forced to free: %s %s\n", temps[i]->who, temps[i]->data);
		//}
		free(temps[i]->data);
		free(temps[i]);
	}

	free(temps);

	temps = NULL;
	temp_count = 0;
}

void free_strings_recent()
{
	int j;

	for(j = temp_count - 1; j >= 0; j--) {
		if(strIsUsed(*temps[j])) {
			if(temps[j]->old_temp) {
				puts("!");
				exit(1);
			}
			free(temps[j]->data);
			free(temps[j]);	
			temp_count -= 1;
		} else {
			break;
		}
	}
}

void free_strings_all()
{
	int new_count = 0;
	int i;

	for(i = 0; i < temp_count; i++) {
		if(strIsUsed(*temps[i])) {
			if(temps[i]->old_temp) {
				puts("!");
				exit(1);
			}
			free(temps[i]->data);
			free(temps[i]);
		} else {
			if(new_count != i) {
				temps[new_count] = temps[i];
			}
			new_count += 1;
		}
	}

	//printf( "temp_count: %i, new_count: %i\n", temp_count, new_count );
	temp_count = new_count;
}

void free_strings_1000()
{
	int new_count = temp_count - 1000;
	int i;

	if(new_count < 0) {
		new_count = 0;
	}

	for(i = new_count; i < temp_count; i++) {
		if(strIsUsed(*temps[i])) {
			if(temps[i]->old_temp) {
				puts("!");
				exit(1);
			}
			free(temps[i]->data);
			free(temps[i]);
		} else {
			if(new_count != i) {
				temps[new_count] = temps[i];
			}
			new_count += 1;
		}
	}

	//printf( "temp_count: %i, new_count: %i\n", temp_count, new_count );
	temp_count = new_count;
}

void free_strings(int force)
{
	if(force) {
		free_strings_final();
		return;
	}

	static int countA = 0;
	static int countB = 0;
	static int countC = 0;

	if(countA == 0) {
		free_strings_recent();
	}

	if(countB == 0) {
		free_strings_1000();
	}

	if(countC == 0) {
		free_strings_all();
	}

	countA += 1;
	if(countA == 250) countA = 0;

	countB += 1;
	if(countB == 2000) countB = 0;

	countC += 1;
	if(countC == 8000) countC = 0;

	return;
}
