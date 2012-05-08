'::::::::
function parse_type_fwd _
	( _
		byref _alias as string _
	) as node_t ptr

	dim as string _real

	_real = lexer_stack.curr_lex->tk_str
	read_token( ) ' dump identifier

	if sym_exists( _alias ) then
		die( "Symbol '" & _alias & "' already exists!" )
	end if

	if sym_exists( _real ) then
		die( "Symbol '" & _real & "' already exists!" )
	end if

	function = tree_node_type_fwd_decl( sym_add_type_fwd( _alias, _real ) )

end function

'::::::::
function parse_type _
	( _
	) as node_t ptr

	dim as string  ident
	dim as integer member_count
	dim as sym_t ptr members = callocate( sizeof( sym_t ) * 256 )
	dim as integer i

	ident = lexer_stack.curr_lex->tk_str
	read_token( ) ' dump identifier

	if match( TK_AS ) then
		free( members )
		return parse_type_fwd( ident )
	end if

	if match( TK_EOL ) = 0 then
		expected( "end of line", lexer_stack.curr_lex->tk_str )
	end if

	if sym_exists( ident ) then
		print "Symbol '" & ident & "' already exists!"
		exit_( 1 )
	end if

	dim as sym_t ptr sym

	sym = sym_add_type( ident, ident, member_count, members )

	while (lexer_stack.curr_lex->tk_typ <> TK_END) and (lexer_stack.curr_lex->tk_typ <> TK_EOF)
		while match( TK_EOL )
			' cycle away the blank lines
		wend
		members[member_count].is_field = -1
		members[member_count].ident = lexer_stack.curr_lex->tk_str
		members[member_count]._alias = lexer_stack.curr_lex->tk_str
		read_token( ) ' dump identifier
		members[member_count].typ = SYM_VAR
		if match( TK_CHAR_LPAREN ) then
			if match_str( "0" ) = 0 then
				expected( "0", lexer_stack.curr_lex->tk_str )
			end if
			if match( TK_TO ) = 0 then
				expected( "TO", lexer_stack.curr_lex->tk_str )
			end if
			members[member_count].is_array = -1
			members[member_count].array_size = val(lexer_stack.curr_lex->tk_str) + 1
			read_token( )
			if match( TK_CHAR_RPAREN ) = 0 then
				expected( "')'", lexer_stack.curr_lex->tk_str )
			end if
		end if
		if match( TK_AS ) = 0 then
			expected( "AS(3)", lexer_stack.curr_lex->tk_str )
		end if
		members[member_count].dt = parse_datatype( )
		if match( TK_EOL ) = 0 then
			expected( "end of line", lexer_stack.curr_lex->tk_str )
		end if
		member_count += 1
	wend

	sym->member_count = member_count

	if match( TK_END ) = 0 then
		expected( "END", lexer_stack.curr_lex->tk_str )
	end if

	if match( TK_TYPE ) = 0 then
		expected( "TYPE", lexer_stack.curr_lex->tk_str )
	end if

	if match( TK_EOL ) = 0 then
		expected( "end of line", lexer_stack.curr_lex->tk_str )
	end if

	function = tree_node_type_decl( sym )

end function
