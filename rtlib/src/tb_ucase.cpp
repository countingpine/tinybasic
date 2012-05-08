#include <ctype.h>

#include "inc/rtlib.h"

TBSTRING& tb$ucase(const TBSTRING& s)
{
	int len = s.len;
	TBSTRING& result = *TBSTRING_alloc_temp(len);
	int pos;

	for(pos = 0; pos < s.len; pos++) {
		((char *)result.data)[pos] = toupper(((char *)s.data)[pos]);
	}

	if(len) {
		((char *)result.data)[len] = 0;
	}

	markIfTemp((TBSTRING *)&s);
	markIfResult((TBSTRING *)&s);

	return result;
}
