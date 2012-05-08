#include once "compiler/src/inc/sym.bi"

enum dt_e
	DT_BAD
	DT_INTEGER
	DT_UINTEGER
	DT_ANY
	DT_TYPE
	DT_STRING
	DT_ZSTRING
	DT_LITSTR
end enum

type datatype_t
	_dt as integer
	_ptr_cnt as integer
	_sym as sym_t ptr
end type
