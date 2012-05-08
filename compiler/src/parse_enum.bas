'::::::::
function parse_enum _
	( _
	) as node_t ptr

	dim as string  ident
	dim as sym_t ptr members = callocate( sizeof( sym_t ) * 512 )
	dim as integer member_count
	dim as integer i

	ident = lexer_stack.curr_lex->tk_str
	read_token( ) ' dump identifier

	if match( TK_EOL ) = 0 then
		expected( "end of line", lexer_stack.curr_lex->tk_str )
	end if

	if sym_exists( ident ) then
		die( "Symbol '" & ident & "' already exists!" )
	end if

	dim as sym_t ptr sym

	sym = sym_add_enum( ident )

	dim as integer curr_val = 0

	while (lexer_stack.curr_lex->tk_typ <> TK_END) and (lexer_stack.curr_lex->tk_typ <> TK_EOF)
		while match( TK_EOL )
			' cycle away the blank lines
		wend
		members[member_count].ident = lexer_stack.curr_lex->tk_str
		members[member_count]._alias = lexer_stack.curr_lex->tk_str
		read_token( ) ' dump identifier
		if match( TK_CHAR_EQ ) then
			dim as node_t ptr expr = parse_expression( )
			if expr->typ <> NODE_LITINT then
				expected( "constant", lexer_stack.curr_lex->tk_str )
			end if
			curr_val = val( expr->_i )
			free(expr)
		end if
		members[member_count].enum_val = curr_val
		sym_add_enum_member( members[member_count].ident, curr_val )
		if match( TK_EOL ) = 0 then
			expected( "end of line1", lexer_stack.curr_lex->tk_str )
		end if
		member_count += 1
		curr_val += 1
	wend

	sym->members = members
	sym->member_count = member_count

	if match( TK_END ) = 0 then
		expected( "END", lexer_stack.curr_lex->tk_str )
	end if

	if match( TK_ENUM ) = 0 then
		expected( "ENUM", lexer_stack.curr_lex->tk_str )
	end if

	if match( TK_EOL ) = 0 then
		expected( "end of line", lexer_stack.curr_lex->tk_str )
	end if

	function = tree_node_enum( sym )

end function
