#include once "compiler/src/inc/sym.bi"
#include once "compiler/src/inc/datatype.bi"

type param_info_t
	sym      as sym_t ptr
	ident    as string
	dt       as datatype_t ptr
	is_byval as integer
	is_byref as integer
end type
