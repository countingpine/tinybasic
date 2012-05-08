'::::::::
sub parse_type _
	( _
	)

	dim as string  ident
	dim as integer member_count
	dim as sym_t ptr members = callocate( sizeof( sym_t ) * 256 )
	dim as integer i

	ident = tk_str
	read_token( ) ' dump identifier

	if match( TK_EOL ) = 0 then
		expected( "end of line", tk_str )
	end if

	while (lcase( tk_str ) <> "end") and (tk_typ <> TK_EOF)
		members[member_count].ident = tk_str
		read_token( ) ' dump identifier
		if match_str( "(" ) then
			members[member_count].typ = SYM_ARRAY
			if match_str( "0" ) = 0 then
				expected( "0", tk_str )
			end if
			if match_str( "to" ) = 0 then
				expected( "TO", tk_str )
			end if
			members[member_count].array_size = val(tk_str) + 1
			read_token( )
			if match_str( ")" ) = 0 then
				expected( "')'", tk_str )
			end if
		else
			members[member_count].typ = SYM_VAR
		end if
		if match_str( "as" ) = 0 then
			expected( "as", tk_str )
		end if
		members[member_count].dt = parse_datatype( )
		if match( TK_EOL ) = 0 then
			expected( "end of line", tk_str )
		end if
		member_count += 1
	wend

	if match_str( "end" ) = 0 then
		expected( "END", tk_str )
	end if

	if match_str( "type" ) = 0 then
		expected( "TYPE", tk_str )
	end if

	if match( TK_EOL ) = 0 then
		expected( "end of line", tk_str )
	end if

	if sym_exists( ident ) then
		print "Symbol '" & ident & "' already exists!"
		exit_( 1 )
	end if

	dim as sym_t ptr sym

	sym = sym_add_type( ident, member_count, members )

	emit_line( "/*::::::::*/" )
	emit_line( "struct " & ident & " {" )
	indent += 1
	i = 0
	while i < member_count
		dim as string s
		dim as datatype_t ptr dt = members[i].dt
		if dt->dt = 0 then
			dt->dt = sym
			dim as datatype_t ptr tmp = sym->members[i].dt
			tmp->dt->dt = sym
		end if
		s = dt->s & " " & members[i].ident
		if members[i].typ = SYM_ARRAY then
			s += "[" & members[i].array_size & "]"
		end if
		emit_line( s & ";" )
		i += 1
	wend
	indent -= 1
	emit_line( "};" )
	emit_line( "" )

end sub

