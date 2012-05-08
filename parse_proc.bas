'::::::::
sub parse_sub _
	( _
		byval is_decl as integer _
	)

	dim as string ident
	dim as datatype_t ptr dt
	dim as string param_ident(0 To 255)
	dim as datatype_t ptr param_dt(0 To 255)
	dim as integer param_is_byval(0 To 255)
	dim as integer param_is_byref(0 To 255)
	dim as integer p_count
	dim as string s
	dim as integer i

	ident = tk_str
	read_token( )

	if match_str( "(" ) = 0 then
		expected( "'('", tk_str )
	end if

	symstack_push( )

	while (tk_str <> ")") and (tk_typ <> TK_EOF)
		if match_str( "byref" ) then
			param_is_byval(p_count) = 0
			param_is_byref(p_count) = -1
		elseif match_str( "byval" ) then
			param_is_byval(p_count) = -1
			param_is_byref(p_count) = 0
		else
			expected( "byval/byref", tk_str )
		end if
		param_ident(p_count) = tk_str
		read_token( )
		if match_str( "as" ) = 0 then
			expected( "as", tk_str )
		end if
		param_dt(p_count) = parse_datatype( )
		sym_add_dim( param_ident(p_count), param_dt(p_count), 0, 0, 0 )
		p_count += 1
		if tk_str = "," then
			read_token( )
		else
			exit while
		end if
	wend

	if match_str( ")" ) = 0 then
		expected( "')'", tk_str )
	end if

	if match_str( "as" ) then
		dt = parse_datatype( )
	else
		dt = 0
	end if

	if match( TK_EOL ) = 0 then
		expected( "end of line", tk_str )
	end if

	emit_line( "/*::::::::*/" )
	if dt then
		s = dt->s & " "
	else
		s = "void "
	end if

	s += ident & "("

	i = 0
	while i < p_count
		if param_is_byref(i) then
			's += "const "
		end if
		s += param_dt(i)->s
		if param_is_byref(i) then
			s += "&"
		end if
		s += " " & param_ident(i)
		i += 1
		if i <> p_count then
			s += ", "
		end if
	wend
       
	s += ")"

	if is_decl then
		emit_line( s & ";" )
	else
		emit_line( s )
		emit_line( "{" )
		indent += 1
		if dt then
			emit_line( dt->s & " func$result = 0;" )
		end if
	end if

	sym_add_proc( ident )

end sub

