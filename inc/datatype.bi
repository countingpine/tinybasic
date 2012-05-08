#include once "inc/sym.bi"

type datatype_t
	dt as sym_t ptr
	ptr_cnt as integer
	s as string
	sym as sym_t ptr
	expr(0 To 15) as any ptr
	expr_cnt as integer
	is_proc as integer
end type

