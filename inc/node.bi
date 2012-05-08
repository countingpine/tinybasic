#include once "inc/datatype.bi"
#include once "inc/sym.bi"

dim shared as integer NODE_BAD      = 0
dim shared as integer NODE_LITINT   = 1
dim shared as integer NODE_LITSTR   = 2
dim shared as integer NODE_BOP      = 3
dim shared as integer NODE_LVALUE   = 4
dim shared as integer NODE_UOP      = 5
dim shared as integer NODE_CONVERT  = 6
dim shared as integer NODE_PROCCALL = 7
dim shared as integer NODE_DIM      = 8
type node_t
	typ as integer
	s as string
	l as node_t ptr
	r as node_t ptr
	dt as datatype_t
	expr(0 To 15) as node_t ptr
	expr_cnt as integer
	sym as sym_t ptr

	dim_sym	as sym_t ptr
	dim_dt	 as datatype_t ptr
	dim_init_expr  as node_t ptr

end type
