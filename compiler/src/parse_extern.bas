'::::::::
function parse_extern _
	( _
	) as node_t ptr

	dim as datatype_t ptr dt
	dim as string ident
	dim as integer is_array
	dim as integer array_size
	dim as sym_t ptr sym

	if match( TK_AS ) = 0 then
		expected( "'AS'", "'" & lexer_stack.curr_lex->tk_str & "'" )
	end if

	dt = parse_datatype( )

	ident = lexer_stack.curr_lex->tk_str
	read_token( ) ' dump ident

	' Will it be an array?
	if match( TK_CHAR_LPAREN ) then
		is_array = -1
		if match_str( "0" ) = 0 then
			expected( "0", lexer_stack.curr_lex->tk_str )
		end if
		if match( TK_TO ) = 0 then
			expected( "TO", lexer_stack.curr_lex->tk_str )
		end if
		array_size = val(lexer_stack.curr_lex->tk_str) + 1
		read_token( ) ' dump ubound
		if match( TK_CHAR_RPAREN ) = 0 then
			expected( "')'", lexer_stack.curr_lex->tk_str )
		end if
	end if

	if match( TK_EOL ) = 0 then
		expected( "end of line", lexer_stack.curr_lex->tk_str )
	end if

	' No symbol with this name?
	if sym_exists( ident ) then
		die( "Symbol '" & ident & "' already exists!" )
	end if

	' Allocate the symbol
	sym = sym_add_extern( ident, dt, is_array, array_size )

	dim as node_t ptr node = callocate( sizeof( node_t ) )

	node->typ = NODE_EXTERN
	node->sym = sym
	node->dt = *dt

	function = node

end function
