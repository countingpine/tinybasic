#include <stdlib.h>

#include "inc/rtlib.h"

int tb$val(const TBSTRING& s)
{
	markIfTemp((TBSTRING *)&s);
	markIfResult((TBSTRING *)&s);

	return atoi((char *)s.data);;
}
