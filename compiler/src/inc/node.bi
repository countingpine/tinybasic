#include once "compiler/src/inc/datatype.bi"
#include once "compiler/src/inc/sym.bi"

enum node_e
	NODE_BAD
' expression nodes
	NODE_LITINT
	NODE_LITSTR
	NODE_BOP
	NODE_LVALUE
	NODE_UOP
	NODE_CONVERT
	NODE_PROCCALL
	NODE_ARRAY_ACCESS
	NODE_VAR
	NODE_PTR_ARRAY_ACCESS
	NODE_ENUM_VAL
' block/stmt nodes
	NODE_DIM
	NODE_FOR
	NODE_LABEL
	NODE_TYPE
	NODE_PROC
	NODE_PROC_DECL
	NODE_STMT_LIST
	NODE_TYPE_DECL
	NODE_TYPE_FWD_DECL
	NODE_DUMMY
	NODE_ENUM
	NODE_ASSIGN
	NODE_IF
	NODE_ELSEIF_LIST
	NODE_ELSEIF
	NODE_ELSE
	NODE_SELFADD
	NODE_SELFSUB
	NODE_ASSIGN_RESULT
	NODE_WHILE
	NODE_RETURN
	NODE_GOTO
	NODE_BREAK
	NODE_PRINT
	NODE_EXTERN
end enum

type list_dim_item_t_fwd as list_dim_item_t
type dim_stmt_t_fwd      as dim_stmt_t

type node_t
	typ as integer
	dt as datatype_t
	sym as sym_t ptr

	l as node_t ptr
	r as node_t ptr

	expr(0 to 31) as node_t ptr
	expr_cnt as integer

	stmt_list as node_t ptr

	_i as string
	_s as string
	_bop as string
	_uop as string

	_data as node_t ptr
	_next as node_t ptr

	dummy as string

	if_expr        as node_t ptr
	if_stmt_list   as node_t ptr
	elseif_list    as node_t ptr
	else_node      as node_t ptr

	index_expr    as node_t ptr
	from_expr     as node_t ptr
	to_expr       as node_t ptr
	for_stmt_list as node_t ptr

	_expr as node_t ptr
	_stmt_list as node_t ptr

	_label as string
	_do_newline as integer

	symtab as symtab_t ptr

	dim_stmt as dim_stmt_t_fwd ptr
end type

type dim_array_bounds_t
	lower_expr as node_t ptr
	upper_expr as node_t ptr
end type

type dim_item_t
	__ident   as string ' temp that is used before the symbol is made
	sym       as sym_t ptr
	dt        as datatype_t ptr
	is_array  as integer
	bounds    as dim_array_bounds_t
	init_expr as node_t ptr
end type

type dim_stmt_t
	is_shared as integer
	item_list as list_dim_item_t_fwd ptr
end type

type list_dim_item_t
	item  as dim_item_t
	_next as list_dim_item_t ptr
end type
