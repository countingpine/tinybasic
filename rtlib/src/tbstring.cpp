#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
//#include <error.h>

#include "inc/rtlib.h"

TBSTRING::TBSTRING()
{
	data = 0;
	len = 0;
	size = 0;
	old_temp = 0;
	flags = 0;
}

TBSTRING::TBSTRING(const TBSTRING& s)
{
	TBSTRING *result = TBSTRING_alloc_temp(s.len);

	if(s.len) {
		memcpy(result->data, s.data, s.len);
		((char *)result->data)[s.len] = 0;
	}

	markIfTemp((TBSTRING *)&s);
	markIfResult((TBSTRING *)&s);

	data = result->data;
	len  = result->len;
	size = result->size;
	old_temp = result;
	flags = 0;
}

TBSTRING::~TBSTRING()
{
	markIfLocal(this);

	data = 0;
	len = 0;
	size = 0;
	old_temp = 0;
	flags = 0;
}

int TBSTRING::operator =(const TBSTRING& s)
{
	//free_strings(0);

	TBSTRING *result = TBSTRING_alloc_temp(s.len);

	if(s.len) {
		memcpy(result->data, s.data, s.len);
		((char *)result->data)[s.len] = 0;
	}

	markIfTemp((TBSTRING *)&s);
	markIfResult((TBSTRING *)&s);

	data = result->data;
	len  = result->len;
	size = result->size;

	if(old_temp) {
		strSetIsUsed(*old_temp);
	}

	old_temp = result;

	return 0;
}

int TBSTRING_EQ(const TBSTRING& lhs, const TBSTRING& rhs)
{
	int result = 1;

	if(lhs.len != rhs.len) {
		result = 0;
	} else {
		if((lhs.len == 0) && (rhs.len == 0)) {
			result = 1;
		} else {
			if((lhs.data == 0) && (rhs.data == 0)) {
				result = 1;
			} else {
				if((lhs.data == 0) || (rhs.data == 0)) {
					result = 0;
				} else {
					if(memcmp(lhs.data, rhs.data, lhs.len) != 0) result = 0;
				}
			}
		}
	}

	markIfTemp((TBSTRING *)&lhs);
	markIfResult((TBSTRING *)&lhs);

	markIfTemp((TBSTRING *)&rhs);
	markIfResult((TBSTRING *)&rhs);

	return result;
}

int TBSTRING_NE(const TBSTRING& lhs, const TBSTRING& rhs)
{
	int result = 0;

	if(lhs.len != rhs.len) {
		result = 1;
	} else {
		if((lhs.len == 0) && (rhs.len == 0)) {
			result = 0;
		} else {
			if((lhs.data == 0) && (rhs.data == 0)) {
				result = 0;
			} else {
				if((lhs.data == 0) || (rhs.data == 0)) {
					result = 1;
				} else {
					if(memcmp(lhs.data, rhs.data, lhs.len) != 0) result = 1;
				}
			}
		}
	}

	markIfTemp((TBSTRING *)&lhs);
	markIfResult((TBSTRING *)&lhs);

	markIfTemp((TBSTRING *)&rhs);
	markIfResult((TBSTRING *)&rhs);

	return result;
}

void TBSTRING_assign(TBSTRING& lhs, const TBSTRING& rhs)
{
	//free_strings(0);

	int len = rhs.len;
	TBSTRING *result = TBSTRING_alloc_temp(len);

	if(len) {
		memcpy(result->data, rhs.data, len);
		((char *)result->data)[len] = 0;
	}

	if(strIsTemp(lhs)) {
		puts("String error, attempt to assign to temp!");
	}

	markIfTemp((TBSTRING *)&rhs);
	markIfResult((TBSTRING *)&rhs);

	lhs.data = result->data;
	lhs.len = result->len;
	lhs.size = result->size;

	if(lhs.old_temp) {
		strSetIsUsed(*(lhs.old_temp));
	}

	lhs.old_temp = result;
}

void TBSTRING_append(TBSTRING& lhs, const TBSTRING& rhs)
{
	free_strings(0);

	int len = lhs.len + rhs.len;
	TBSTRING *result = TBSTRING_alloc_temp(len);

	if(lhs.len) {
		memcpy(result->data, lhs.data, lhs.len);
	}
	if(rhs.len) {
		memcpy(&(((char *)result->data)[lhs.len]), rhs.data, rhs.len);
	}
	if(len) {
		((char *)result->data)[len] = 0;
	}

	if(strIsTemp(lhs)) {
		puts("String error, attempt to append to temp!");
	}

	markIfTemp((TBSTRING *)&rhs);
	markIfResult((TBSTRING *)&rhs);

	lhs.data = result->data;
	lhs.len = result->len;
	lhs.size = result->size;

	if(lhs.old_temp) {
		strSetIsUsed(*(lhs.old_temp));
	}

	lhs.old_temp = result;

	return;
}
