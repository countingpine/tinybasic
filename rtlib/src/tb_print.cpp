#include <stdio.h>

#include "inc/rtlib.h"

void tb$print(const TBSTRING& s, int nl)
{
	int pos;

	for(pos = 0; pos < s.len; pos++) {
		fputc(((char *)s.data)[pos], stdout);
	}

	if(nl) fputc(10, stdout);

	markIfTemp((TBSTRING *)&s);
	markIfResult((TBSTRING *)&s);
}
