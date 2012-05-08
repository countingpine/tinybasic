#include once "compiler/src/inc/param_info.bi"

'::::::::
sub symstack_init _
	( _
	)

	symstack.stk(0) = callocate( sizeof( symtab_t ) )

end sub

'::::::::
sub symstack_deinit _
	( _
	)

	symtab_destroy(symstack.stk(0))
	free(symstack.stk(0))
	symstack.stk(0) = 0
	symstack.stk_p = 0

end sub

'::::::::
sub symstack_push _
	( _
	)

	symstack.stk_p += 1
	symstack.stk(symstack.stk_p) = callocate( sizeof( symtab_t ) )

end sub

'::::::::
function symstack_pop _
	( _
	) as symtab_t ptr

	function = symstack.stk(symstack.stk_p)
	symstack.stk_p -= 1

end function

'::::::::
function sym_exists _
	( _
		byref ident as string _
	) as integer

	dim as integer i

	dim as string ucase_ident = ucase( ident )

	for i = 0 to symstack.stk(symstack.stk_p)->sym_count - 1
		if symstack.stk(symstack.stk_p)->syms(i).ident = ucase_ident then
			return -1
		end if
	next i

end function

function sym_find_field _
	( _
		byref ident as string, _
		byval sym   as sym_t ptr _
	) as sym_t ptr

	dim as integer i

	dim as string ucase_ident = ucase( ident )

	while i < sym->member_count
		if sym->members[i].ident = ucase_ident then
			return @sym->members[i]
		end if
		i += 1
	wend

end function

'::::::::
function sym_find _
	( _
		byref ident as string, _
		byval sym   as sym_t ptr _
	) as sym_t ptr

	dim as integer i
	dim as integer p = symstack.stk_p

	if sym then
		return sym_find_field( ident, sym )
	end if

	dim as string ucase_ident = ucase( ident )

	while p >= 0
		i = 0
		while i < symstack.stk(p)->sym_count
			if symstack.stk(p)->syms(i).ident = ucase_ident then
				return @symstack.stk(p)->syms(i)
			end if
			i += 1
		wend
		p -= 1
	wend

end function

'::::::::
function sym_new _
	( _
		byval typ       as integer, _
		byref ident     as string, _
		byval is_global as integer _
	) as sym_t ptr

	dim as integer   stk_p
	dim as integer   _pos
	dim as sym_t ptr sym

	if is_global then
		stk_p = 0
	else
		stk_p = symstack.stk_p
	end if

	_pos = symstack.stk(stk_p)->sym_count

	sym = @symstack.stk(stk_p)->syms(_pos)
	symstack.stk(stk_p)->sym_count += 1

	sym->typ   = typ
	sym->ident = ucase( ident )
	sym->_alias = ucase( ident )

	function = sym

end function

'::::::::
function sym_add_type _
	( _
		byref ident        as string, _
		byref _alias       as string, _
		byval member_count as integer, _
		byval members      as sym_t ptr _
	) as sym_t ptr

	dim as sym_t ptr sym = sym_new( SYM_DATATYPE, ident, -1 )
	dim as datatype_t ptr dt

	sym->member_count = member_count
	sym->members      = members
	sym->dt           = callocate( sizeof( datatype_t ) )
	sym->_alias       = _alias

	dt = sym->dt
	dt->_dt  = DT_TYPE
	dt->_sym = sym

	function = sym

end function

'::::::::
function sym_add_type_fwd _
	( _
		byref _alias as string, _
		byref _real  as string _
	) as sym_t ptr

	dim as sym_t ptr sym = sym_new( SYM_DATATYPE_FWD, _alias, -1 )

	sym->_real = _real

	function = sym

end function

'::::::::
function sym_add_enum _
	( _
		byref ident as string _
	) as sym_t ptr

	function = sym_new( SYM_ENUM, ident, -1 )

end function

'::::::::
function sym_add_enum_member _
	( _
		byref ident    as string, _
		byval enum_val as integer _
	) as sym_t ptr

	dim as sym_t ptr sym = sym_new( SYM_ENUM_MEMBER, ident, -1 )

	sym->enum_val = enum_val

	function = sym

end function

'::::::::
function sym_add_proc _
	( _
		byref ident   as string, _
		byref _alias  as string, _
		byval dt      as datatype_t ptr, _
		byval pi      as param_info_t ptr, _
		byval p_count as integer _
	) as sym_t ptr

	dim as sym_t ptr sym = sym_new( SYM_PROC, ident, -1 )

	sym->dt      = dt
	sym->is_proc = -1
	sym->pi      = pi
	sym->p_count = p_count
	sym->_alias  = _alias

	function = sym

end function

'::::::::
function sym_add_dim _
	( _
		byref ident      as string, _
		byref _alias     as string, _
		byval dt         as datatype_t ptr, _
		byval is_array   as integer, _
		byval array_size as integer, _
		byval is_shared  as integer _
	) as sym_t ptr

	dim as sym_t ptr sym = sym_new( SYM_VAR, ident, 0 )

	sym->dt         = dt
	sym->is_array   = is_array
	sym->array_size = array_size
	sym->is_shared  = is_shared
	sym->_alias     = _alias

	function = sym

end function

'::::::::
function sym_add_extern _
	( _
		byref ident      as string, _
		byval dt         as datatype_t ptr, _
		byval is_array   as integer, _
		byval array_size as integer _
	) as sym_t ptr

	dim as sym_t ptr sym = sym_new( SYM_EXTERN_VAR, ident, 0 )

	sym->dt         = dt
	sym->is_array   = is_array
	sym->array_size = array_size

	function = sym

end function

'::::::::
function sym_add_label _
	( _
		byref ident as string _
	) as sym_t ptr

	function = sym_new( SYM_LABEL, ident, 0 )

end function


sub datatype_destroy _
	( _
		byval dt as datatype_t ptr _
	)

end sub

sub param_info_destroy _
	( _
		byval pi as param_info_t ptr _
	)

	'sym      as sym_t ptr
	'ident    as string
	'dt       as datatype_t ptr
	'is_byval as integer
	'is_byref as integer

	pi->ident = ""
	free(pi->dt)
	pi->dt = 0

end sub

sub sym_destroy _
	( _
		byval sym as sym_t ptr _
	)

	'typ          as integer
	'ident        as string
	'dt           as any ptr
	'is_array     as integer
	'is_shared    as integer
	'is_proc      as integer
	'is_field     as integer
	'array_size   as integer
	'members      as sym_t ptr
	'member_count as integer
	'enum_val     as integer
	'pi           as any ptr
	'p_count      as integer

	dim as integer i

	'print sym->ident
	sym->ident = ""
	free( sym->dt )
	sym->dt = 0
	
	i = 0
	while i < sym->member_count
		sym_destroy( @sym->members[i] )
		i += 1
	wend

	free( sym->members )

	i = 0
	while i < sym->p_count
		dim as param_info_t ptr pi = sym->pi
		param_info_destroy( @pi[i] )
		i += 1
	wend
	
	free( sym->pi )

end sub

sub symtab_destroy _
	( _
		byval symtab as symtab_t ptr _
	)

	'syms(0 to 1023) as sym_t
	'sym_count as integer

	dim as integer i

	while i < symtab->sym_count
		sym_destroy( @symtab->syms(i) )
		i += 1
	wend

end sub


