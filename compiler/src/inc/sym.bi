enum sym_e
	SYM_BAD
	SYM_DATATYPE
	SYM_DATATYPE_FWD
	SYM_VAR
	SYM_PROC
	SYM_LABEL
	SYM_ENUM
	SYM_ENUM_MEMBER
	SYM_EXTERN_VAR
end enum

type param_info_t_fwd as param_info_t
type datatype_t_fwd   as datatype_t

type sym_t
	typ          as integer
	ident        as string
	_alias       as string
	dt           as datatype_t_fwd ptr
	is_array     as integer
	is_shared    as integer
	is_proc      as integer
	is_field     as integer
	array_size   as integer
	members      as sym_t ptr
	member_count as integer
	enum_val     as integer
	pi           as param_info_t_fwd ptr
	p_count      as integer
	freed        as integer
	_real        as string
end type

type symtab_t
	syms(0 to 1023) as sym_t
	sym_count as integer
end type

type symstack_t
	stk(0 to 63) as symtab_t ptr
	stk_p as integer
end type

declare sub sym_destroy _
	( _
		byval sym as sym_t ptr _
	)

declare sub symtab_destroy _
	( _
		byval symtab as symtab_t ptr _
	)
