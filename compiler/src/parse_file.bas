function parse_rem _
	( _
	) as node_t ptr

	while (lexer_stack.curr_lex->tk_typ <> TK_EOL) and (lexer_stack.curr_lex->tk_typ <> TK_EOF)
		read_token( )
	wend

	if match( TK_EOL ) = 0 then
		expected( "end of line", lexer_stack.curr_lex->tk_str )
	end if

	function = tree_node_dummy( "parse_rem" )

end function

function parse_stmt _
	( _
	) as node_t ptr

	dim as integer no_semi

	dim as node_t ptr result
	dim as string temp

	temp = lexer_stack.curr_lex->tk_str

	if match( TK_CHAR_HASH ) then
		result = parse_pp( )
	elseif match( TK_REM ) then
		result = parse_rem( )
	elseif lexer_stack.curr_lex->tk_typ = TK_DIM then
		result = parse_dim( )
	elseif lexer_stack.curr_lex->tk_typ = TK_FOR then
		result = parse_for( )
	elseif match( TK_EXTERN ) then
		result = parse_extern( )
	elseif match( TK_TYPE ) then
		result = parse_type( )
	elseif match( TK_ENUM ) then
		result = parse_enum( )
	elseif match( TK_DECLARE ) then
		if match( TK_SUB ) then
			result = parse_proc( 0, -1 )
		elseif match( TK_FUNCTION ) then
			result = parse_proc( -1, -1 )
		else
			expected( "SUB/FUNCTION", lexer_stack.curr_lex->tk_str )
		end if
	elseif match( TK_SUB ) then
		result = parse_proc( 0, 0 )
	elseif match( TK_FUNCTION ) then
		if match( TK_CHAR_EQ ) then
			dim as node_t ptr expr
			expr = parse_expression( )
			if match( TK_EOL ) = 0 then
				expected( "end of line", lexer_stack.curr_lex->tk_str )
			end if
			result = tree_node_assign_result( expr )
			'emit_line( "func$result = " & expr_to_str( expr ) & ";" )
		else
			result = parse_proc( -1, 0 )
		end if
	elseif lexer_stack.curr_lex->tk_typ = TK_PRINT then
		dim as node_t ptr expr
		dim as integer has_semi
		read_token( )
		expr = parse_expression( )
		has_semi = match( TK_CHAR_SEMICOLON )
		if match( TK_EOL ) = 0 then
			expected( "end of line", lexer_stack.curr_lex->tk_str )
		end if
		dim as node_t ptr nl_expr
		if has_semi then
			nl_expr = tree_node_litint( "0" )
		else
			nl_expr = tree_node_litint( "1" )
		end if
		dim as sym_t ptr print_sym = sym_find( "tb_print", 0 )
		result = tree_node_proccall( print_sym, 0, 0 )
		result->expr(0) = expr
		result->expr(1) = nl_expr
		result->expr_cnt = 2
	elseif match( TK_RETURN ) then
		dim as node_t ptr expr
		expr = parse_expression( )
		if match( TK_EOL ) = 0 then
			expected( "end of line", lexer_stack.curr_lex->tk_str )
		end if
		result = tree_node_return( expr )
	elseif match( TK_GOTO ) then
		dim as string s
		s = lexer_stack.curr_lex->tk_str
		read_token( )
		if match( TK_EOL ) = 0 then
			expected( "end of line", lexer_stack.curr_lex->tk_str )
		end if
		result = tree_node_goto( s )
	elseif match( TK_WHILE ) then
		result = parse_while( )
	elseif match( TK_IF ) then
		result = parse_if( )
	elseif match( TK_DO ) then
		if match( TK_EOL ) = 0 then
			expected( "end of line", lexer_stack.curr_lex->tk_str )
		end if
		'emit_line( "do {" )
		indent += 1
		parse_stmt_list( )
		die( "do broken.." )
	elseif match( TK_EXIT ) then
		read_token( )
		if match( TK_EOL ) = 0 then
			expected( "end of line", lexer_stack.curr_lex->tk_str )
		end if
		'emit_line( "break;" )
		result = tree_node_break( )
	elseif (lexer_stack.curr_lex->tk_typ = TK_IDENT) or (lexer_stack.curr_lex->tk_typ = TK_CHAR_ASTERISK) then
		dim as node_t ptr lvalue = parse_addrof_deref( ) ' start at the top where lvalue is?
		dim as node_t ptr expr

		if match( TK_CHAR_EQ ) then
			expr = parse_expression( )
			if match( TK_EOL ) = 0 then
				expected( "end of line", lexer_stack.curr_lex->tk_str )
			end if
			result = tree_node_assign( lvalue, expr )
		elseif match( TK_SELFADD ) then
			expr = parse_expression( )
			if match( TK_EOL ) = 0 then
				expected( "end of line", lexer_stack.curr_lex->tk_str )
			end if
			result = tree_node_selfadd( lvalue, expr )
		elseif match( TK_SELFSUB ) then
			expr = parse_expression( )
			if match( TK_EOL ) = 0 then
				expected( "end of line", lexer_stack.curr_lex->tk_str )
			end if
			result = tree_node_selfsub( lvalue, expr )
		elseif match( TK_CHAR_COLON ) then
			if match( TK_EOL ) = 0 then
				expected( "end of line", lexer_stack.curr_lex->tk_str )
			end if
			die( "Unhandled Label" )
		else
			' function call
			if match( TK_EOL ) = 0 then
				expected( "end of line", lexer_stack.curr_lex->tk_str )
			end if
			result = lvalue
		end if
	elseif match( TK_EOL ) then
		'' nothing
		result = tree_node_dummy( "<EOL>" )
	else
		expected( "keyword", lexer_stack.curr_lex->tk_str )
	end if
	
	if result = 0 then
		die( temp & " not handled!" )
	end if

	function = result


end function

'::::::::
function parse_stmt_list _
	( _
	) as node_t ptr

	dim as node_t ptr node

	while lexer_stack.curr_lex->tk_typ <> TK_EOF
		if match( TK_END ) then
			if match( TK_SUB ) then
				'emit_line( "return;" )
			elseif match( TK_FUNCTION ) then
				'emit_line( "return func$result;" )
			elseif lexer_stack.curr_lex->tk_typ = TK_IF then
				exit while
			else
				expected( "keyword", lexer_stack.curr_lex->tk_str )
			end if
			if match( TK_EOL ) = 0 then
				expected( "end of line", lexer_stack.curr_lex->tk_str )
			end if
			'indent -= 1
			'emit_line( "}" )
			exit while
		elseif match( TK_WEND ) then
			if match( TK_EOL ) = 0 then
				expected( "end of line", lexer_stack.curr_lex->tk_str )
			end if
			exit while
		elseif lexer_stack.curr_lex->tk_typ = TK_NEXT then
			exit while
		elseif match( TK_LOOP ) then
			if match( TK_EOL ) = 0 then
				expected( "end of line", lexer_stack.curr_lex->tk_str )
			end if
			'indent -= 1
			'emit_line( "}" )
			exit while
		elseif lexer_stack.curr_lex->tk_typ = TK_ELSEIF then
			exit while
		elseif lexer_stack.curr_lex->tk_typ = TK_ELSE then
			exit while
		end if
		node = tree_node_stmt_list( node, parse_stmt( ) )
	wend

	function = node

end function

'::::::::
function parse_file _
	( _
	) as node_t ptr

	function = parse_stmt_list( )

end function
