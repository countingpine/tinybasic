'::::::::
sub symstack_push _
	( _
	)

	symstack.stk_p += 1
	symstack.stk(symstack.stk_p).sym_count = 0

end sub

'::::::::
sub symstack_pop _
	( _
	)

	symstack.stk_p -= 1

end sub

'::::::::
function sym_exists _
	( _
		byref ident as string _
	) as integer

	dim as integer i

	i = 0
	while i < symstack.stk(symstack.stk_p).sym_count
		if lcase( symstack.stk(symstack.stk_p).syms(i).ident ) = lcase( ident ) then
			Return -1
		end if
		i += 1
	wend

end function

'::::::::
function sym_find _
	( _
		byref ident as string, _
		byval sym as sym_t ptr _
	) as sym_t ptr

	dim as integer i
	dim as integer p = symstack.stk_p

	'print __FUNCTION__

	if sym = 0 then
		while p >= 0
			i = 0
			while i < symstack.stk(p).sym_count
				'print lcase( symstack.stk(p).syms(i).ident ) & " " & lcase( ident )
				if lcase( symstack.stk(p).syms(i).ident ) = lcase( ident ) then
					Return @symstack.stk(p).syms(i)
				end if
				i += 1
			wend
			p -= 1
		wend
	else
		dim as datatype_t ptr dt = sym->dt
		if dt then
			if dt->dt then
				i = 0
				while i < dt->dt->member_count
					'print lcase( dt->dt->members[i].ident ) & " " & lcase( ident )
					if lcase( dt->dt->members[i].ident ) = lcase( ident ) then
						Return @dt->dt->members[i]
					end if
					i += 1
				wend
			end if
		end if
		if sym->dt_parent then
			print "not yet done!"
		end if
	end if

end function

'::::::::
function sym_add_type _
	( _
		byref ident as string, _
		byval member_count as integer, _
		byval members as sym_t ptr _
	) as sym_t ptr

	dim as integer _pos = symstack.stk(0).sym_count

	symstack.stk(0).syms(_pos).typ = SYM_DATATYPE
	symstack.stk(0).syms(_pos).ident = ident
	symstack.stk(0).syms(_pos).member_count = member_count
	symstack.stk(0).syms(_pos).members = members

	function = @symstack.stk(0).syms(_pos)

	symstack.stk(0).sym_count += 1

end function

'::::::::
function sym_add_proc _
	( _
		byref ident as string _
	) as sym_t ptr

	dim as integer _pos = symstack.stk(0).sym_count

	symstack.stk(0).syms(_pos).typ = SYM_PROC
	symstack.stk(0).syms(_pos).ident = ident
	symstack.stk(0).syms(_pos).dt = callocate( sizeof( datatype_t ) )

	dim as datatype_t ptr dt = symstack.stk(0).syms(_pos).dt
	dt->s = "<PROC>"

	function = @symstack.stk(0).syms(_pos)

	symstack.stk(0).sym_count += 1

end function

'::::::::
function sym_add_dim _
	( _
		byref ident as string, _
		byval dt as datatype_t ptr, _
		byval is_array as integer, _
		byval array_size as integer, _
		byval is_shared as integer _
	) as sym_t ptr

	dim as integer _pos = symstack.stk(symstack.stk_p).sym_count
	dim as sym_t ptr dt_parent

	dt_parent = sym_find( dt->s, 0 )

	if is_array then
		symstack.stk(symstack.stk_p).syms(_pos).typ = SYM_ARRAY
	else
		symstack.stk(symstack.stk_p).syms(_pos).typ = SYM_VAR
	end if
	symstack.stk(symstack.stk_p).syms(_pos).ident = ident
	symstack.stk(symstack.stk_p).syms(_pos).dt = dt
	symstack.stk(symstack.stk_p).syms(_pos).is_array = is_array
	symstack.stk(symstack.stk_p).syms(_pos).array_size = array_size
	symstack.stk(symstack.stk_p).syms(_pos).is_shared = is_shared
	symstack.stk(symstack.stk_p).syms(_pos).dt_parent = dt_parent

	function = @symstack.stk(symstack.stk_p).syms(_pos)

	symstack.stk(symstack.stk_p).sym_count += 1

end function

'::::::::
sub sym_add_label _
	( _
		byref ident as string _
	)

	dim as integer _pos = symstack.stk(symstack.stk_p).sym_count

	symstack.stk(symstack.stk_p).syms(_pos).typ = SYM_LABEL
	symstack.stk(symstack.stk_p).syms(_pos).ident = ident

	symstack.stk(symstack.stk_p).sym_count += 1

end sub

