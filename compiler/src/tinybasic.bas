#include once "crt.bi"
#undef rem
#define rem

#include once "inc/consts.bi"

#include once "inc/sym.bi"
#include once "inc/datatype.bi"


#include once "inc/node.bi"

#include once "inc/tree.bi"

#include once "inc/keywords.bi"

'::::::::
declare sub parse_file _
	( _
	)

dim shared as symstack_t symstack

'*******************************************************************************
' Emit
'*******************************************************************************



'*******************************************************************************
' Error
'*******************************************************************************

'*******************************************************************************
' Symbol table
'*******************************************************************************

#include "sym.bas"

'*******************************************************************************
' Lexer
'*******************************************************************************

#include "lex.bas"

'*******************************************************************************
' Tree
'*******************************************************************************

#include "tree.bas"

'*******************************************************************************
' ???
'*******************************************************************************

#include "help.bas"

'*******************************************************************************
' Parser
'*******************************************************************************

#include once "emit.bas"
#include once "parse.bas"
#include once "parse_pp.bas"
#include once "parse_file.bas"

'*******************************************************************************
' Main
'*******************************************************************************

'::::::::
function main _
	( _
		byval argc as integer, _
		byval argv as zstring ptr ptr _
	) as integer

	print "#include <stdlib.h>"
	print "#include <stdio.h>"
	print "#include <string.h>"
	print "#include <ctype.h>"
	print "struct FBSTRING {"
	print !"\tFBSTRING();"
	print !"\tFBSTRING(const char *);"
	print !"\tFBSTRING(const FBSTRING&);"
	print !"\t~FBSTRING();"
	print !"\tint operator=(const char*);"
	print !"\tint operator=(const FBSTRING&);"
	print !"\tint operator==(const FBSTRING&);"
	print !"\tint operator!=(const FBSTRING&);"
	print !"\tint operator+=(const FBSTRING&);"
	'print !"\toperator const FBSTRING&();"
	'print !"\toperator const FBSTRING();"
	'print !"\toperator FBSTRING&();"
	'print !"\toperator FBSTRING();"
	print !"\tvoid *data;"
	print !"\tint len;"
	print !"\tint size;"
	print "};"
	print "typedef int integer;"
	print "typedef char zstring;"
	print "void print(const FBSTRING&, integer nl);"
	print "FBSTRING& concat(const FBSTRING&, const FBSTRING&);"
	print "FBSTRING& lcase(const FBSTRING&);"
	print "FBSTRING& int_to_str(integer);"
	print "FBSTRING& chr(integer);"
	print "integer asc(const FBSTRING&);"
	print "integer val(const FBSTRING&);"
	print "#define exit_ exit"
	print "#define EOF_ EOF"
	print "#define callocate(n) calloc(n, 1 )"
       
	' CONSTRUCTORS

	print "FBSTRING::FBSTRING()"
	print "{"
	print !"\tdata = 0;"
	print !"\tlen = 0;"
	print !"\tsize = 0;"
	print "}"

	print "FBSTRING::FBSTRING(const char *s)"
	print "{"
	print !"\tdata = 0;"
	print !"\tlen = 0;"
	print !"\tsize = 0;"
	print !"\tif(s == 0) return;"
	print !"\tint s_len = strlen(s);"
	print !"\tdata = malloc(s_len);"
	print !"\tlen = s_len;"
	print !"\tsize = s_len;"
	print !"\tmemcpy(data, s, s_len);"
	print "}"

	print "FBSTRING::FBSTRING(const FBSTRING& s)"
	print "{"
	print !"\tdata = 0;"
	print !"\tlen = 0;"
	print !"\tsize = 0;"
	print !"\tif(s.data == 0) return;"
	print !"\tdata = malloc(s.len);"
	print !"\tlen = s.len;"
	print !"\tsize = s.len;"
	print !"\tmemcpy(data, s.data, s.len);"
	print "}"

	' DTOR

	print "FBSTRING::~FBSTRING()"
	print "{"
	'print !"\tif(data) free(data);"
	print !"\tdata = 0;"
	print !"\tlen = 0;"
	print !"\tsize = 0;"
	print "}"

	' LET

	print "int FBSTRING::operator=(const char *s)"
	print "{"
	print !"\tif(data) free(data);"
	print !"\tdata = 0;"
	print !"\tlen = 0;"
	print !"\tsize = 0;"
	print !"\tif(s == 0) return 0;"
	print !"\tint s_len = strlen(s);"
	print !"\tdata = malloc(s_len);"
	print !"\tlen = s_len;"
	print !"\tsize = s_len;"
	print !"\tmemcpy(data, s, s_len);"
	print !"\treturn 0;"
	print "}"

	print "int FBSTRING::operator=(const FBSTRING& s)"
	print "{"
	'print !"\tif(data) free(data);"
	print !"\tdata = 0;"
	print !"\tlen = 0;"
	print !"\tsize = 0;"
	print !"\tif(s.data == 0) return 0;"
	print !"\tdata = malloc(s.len);"
	print !"\tlen = s.len;"
	print !"\tsize = s.len;"
	print !"\tmemcpy(data, s.data, s.len);"
	print !"\treturn 0;"
	print "}"

	' REL OP

	print !"int FBSTRING::operator==(const FBSTRING& s)"
	print "{"
	print !"\tif(len != s.len) return 0;"
	print !"\tif((data == 0) && (s.data == 0)) return 1;"
	print !"\tif((data == 0) || (s.data == 0)) return 0;"
	print !"\tif(memcmp(data, s.data, len) != 0) return 0;"
	print !"\treturn 1;"
	print "}"

	print !"int FBSTRING::operator!=(const FBSTRING& s)"
	print "{"
	print !"\tif(len != s.len) return 1;"
	print !"\tif((data == 0) && (s.data == 0)) return 0;"
	print !"\tif((data == 0) || (s.data == 0)) return 1;"
	print !"\tif(memcmp(data, s.data, len) != 0) return 1;"
	print !"\treturn 0;"
	print "}"

	' SELF OP

	print !"int FBSTRING::operator+=(const FBSTRING& s)"
	print "{"
	print !"\tFBSTRING result;"
	print !"\tresult.data = data;"
	print !"\tresult.len = len;"
	print !"\tresult.size = size;"
	print !"\tresult = concat(result, s);"
	print !"\tfree(data);"
	print !"\tdata = result.data;"
	print !"\tlen = result.len;"
	print !"\tsize = result.size;"
	print !"\treturn 0;"
	print "}"




	print "void print(const FBSTRING& s, integer nl)"
	print "{"
	print !"\tint pos;"
	print !"\tfor(pos = 0; pos < s.len; pos++) {"
	print !"\t\tfputc(((char *)s.data)[pos], stdout);"
	print !"\t}"
	print !"\tif(nl) fputc(10, stdout);"
	print "}"

	print "FBSTRING& concat(const FBSTRING& s1, const FBSTRING& s2)"
	print "{"
	print !"\tFBSTRING *result = (FBSTRING *)calloc(sizeof(FBSTRING), 1);"
	print !"\tresult->data = malloc(s1.len + s2.len);"
	print !"\tresult->len = s1.len + s2.len;"
	print !"\tresult->size = s1.len + s2.len;"
	print !"\tmemcpy(result->data, s1.data, s1.len);"
	print !"\tmemcpy(&(((char *)result->data)[s1.len]), s2.data, s2.len);"
	print !"\treturn *result;"
	print "}"

	print "FBSTRING& lcase(const FBSTRING& s)"
	print "{"
	print !"\tFBSTRING *result = (FBSTRING *)calloc(sizeof(FBSTRING), 1);"
	print !"\t*result = s;"
	print !"\tint pos;"
	print !"\tfor(pos = 0; pos < s.len; pos++) {"
	print !"\t\t((char *)result->data)[pos] = tolower(((char *)s.data)[pos]);"
	print !"\t}"
	print !"\treturn *result;"
	print "}"

	print "FBSTRING& str_temp(const FBSTRING& s)"
	print "{"
	print !"\tFBSTRING *result = (FBSTRING *)calloc(sizeof(FBSTRING), 1);"
	print !"\t*result = s;"
	print !"\treturn *result;"
	print "}"

	print "FBSTRING& int_to_str(integer i)"
	print "{"
	print !"\tFBSTRING *result = (FBSTRING *)calloc(sizeof(FBSTRING), 1);"
	print !"\tchar *tmp = (char *)calloc(32, 1);"
	print !"\tsprintf(tmp, " & chr( 34 ) & "%i" & chr( 34 ) & ", i);"
	print !"\tint tmp_len = strlen(tmp);"
	print !"\tresult->data = tmp;"
	print !"\tresult->len = tmp_len;"
	print !"\tresult->size = 32;"
	print !"\treturn *result;"
	print "}"

	print "FBSTRING& chr(integer i)"
	print "{"
	print !"\tFBSTRING *result = (FBSTRING *)calloc(sizeof(FBSTRING), 1);"
	print !"\tresult->data = malloc(1);"
	print !"\tresult->len = 1;"
	print !"\tresult->size = 1;"
	print !"\t((char *)result->data)[0] = i;"
	print !"\treturn *result;"
	print "}"

	print "integer val(const FBSTRING& s)"
	print "{"
	print !"\tint n;"
	print !"\tchar *tmp = (char *)calloc(s.len + 1, 1);"
	print !"\tmemcpy(tmp, s.data, s.len);"
	print !"\tn = atoi(tmp);"
	print !"\tfree(tmp);"
	print !"\treturn n;"
	print "}"

	print "integer asc(const FBSTRING& s)"
	print "{"
	print !"\tif(s.data == 0) return -1;"
	print !"\treturn ((char *)s.data)[0];"
	print "}"

	sym_add_proc( "lcase" )
	sym_add_proc( "exit_" )
	sym_add_proc( "EOF_" )
	sym_add_proc( "fgetc" )
	sym_add_proc( "asc" )
	sym_add_proc( "isdigit" )
	sym_add_proc( "isalnum" )
	sym_add_proc( "isalpha" )
	sym_add_proc( "callocate" )
	sym_add_proc( "sizeof" )
	sym_add_proc( "chr" )
	sym_add_proc( "val" )
	dim as datatype_t ptr dt = callocate( sizeof( datatype_t ) )
	dt->s = "integer"
	sym_add_dim( "__FUNCTION__", dt, 0, 0, -1 )
	sym_add_dim( "stdin", dt, 0, 0, -1 )

	read_char( )
	read_token( )
       
	parse_file( )
       
	if tk_typ <> TK_EOF then
		print "FAILED!"
	end if

	function = 0

end function

#include once "error.bas"
#include once "consts.bas"

'::::::::
rem main( __FB_ARGC__, __FB_ARGV__ )
