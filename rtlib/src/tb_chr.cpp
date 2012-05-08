#include "inc/rtlib.h"

TBSTRING& tb$chr(int i)
{
	TBSTRING *result = TBSTRING_alloc_temp(1);

	((char *)result->data)[0] = i;
	((char *)result->data)[1] = 0;

	return *result;
}
