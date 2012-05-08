#ifndef __RTLIB_H__
#define __RTLIB_H__

#include "tbstring.h"

int tb$asc(const TBSTRING& s);
TBSTRING& tb$chr(int i);
TBSTRING& tb$lcase(const TBSTRING& s);
void tb$print(const TBSTRING& s, int nl);
TBSTRING& tb$ucase(const TBSTRING& s);
int tb$val(const TBSTRING& s);



TBSTRING& concat(const TBSTRING& s1, const TBSTRING& s2);

TBSTRING& zstr_temp(char *s);

TBSTRING& int_to_str(int i);

char *TBSTRING_to_zstrp(const TBSTRING& s);

TBSTRING *TBSTRING_alloc_temp(int len);

void free_strings(int force);

void markIfTemp(TBSTRING *s);

void markIfResult(TBSTRING *s);

void markIfLocal(TBSTRING *s);

#endif
