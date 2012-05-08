dim shared as integer SYM_BAD      = 0
dim shared as integer SYM_DATATYPE = 1
dim shared as integer SYM_VAR      = 2
dim shared as integer SYM_PROC     = 3
dim shared as integer SYM_ARRAY    = 4
dim shared as integer SYM_LABEL    = 5

type sym_t
	typ as integer
	ident as string
	dt as any ptr
	is_array as integer
	is_shared as integer
	array_size as integer
	dt_parent as sym_t ptr
	members as sym_t ptr
	member_count as integer
end type

type symtab_t
	syms(0 To 255) as sym_t
	sym_count as integer
end type

type symstack_t
	stk(0 To 15) as symtab_t
	stk_p as integer
end type

