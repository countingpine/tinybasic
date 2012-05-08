#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
//#include <error.h>

#include "inc/rtlib.h"

TBSTRING& concat(const TBSTRING& lhs, const TBSTRING& rhs)
{
	int len = lhs.len + rhs.len;
	TBSTRING& result = *TBSTRING_alloc_temp(len);

	if(lhs.len) {
		memcpy(result.data, lhs.data, lhs.len);
	}
	if(rhs.len) {
		memcpy(&(((char *)result.data)[lhs.len]), rhs.data, rhs.len);
	}
	if(len) {
		((char *)result.data)[len] = 0;
	}

	markIfTemp((TBSTRING *)&lhs);
	markIfResult((TBSTRING *)&lhs);

	markIfTemp((TBSTRING *)&rhs);
	markIfResult((TBSTRING *)&rhs);

	return result;
}

TBSTRING& zstr_temp(char *s)
{
	TBSTRING *result = TBSTRING_alloc_temp(0);

	if(s) {
		int s_len = strlen(s);
		if(s_len) {
			result->len = s_len;
			result->size = s_len + 1;
			result->data = calloc(s_len + 1, 1);
			strcpy((char *)result->data, s);
		}
	}

	return *result;
}

TBSTRING& int_to_str(int i)
{
	TBSTRING *result = TBSTRING_alloc_temp(32);

	sprintf((char *)result->data, "%i", i);

	result->len = strlen((char *)result->data);
	result->size = 32;

	return *result;
}

char *TBSTRING_to_zstrp(const TBSTRING& s)
{
	markIfTemp((TBSTRING *)&s);
	markIfResult((TBSTRING *)&s);

	return (char *)s.data;
}
