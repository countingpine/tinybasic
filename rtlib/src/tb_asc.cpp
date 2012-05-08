#include "inc/rtlib.h"

int tb$asc(const TBSTRING& s)
{
	markIfTemp((TBSTRING *)&s);
	markIfResult((TBSTRING *)&s);

	if(s.data == 0) return -1;

	return ((char *)s.data)[0];
}
