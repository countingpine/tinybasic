'::::::::
function parse_for _
	( _
	) as node_t ptr

	dim as node_t ptr index_expr
	dim as node_t ptr from_expr
	dim as node_t ptr to_expr
	dim as node_t ptr for_stmt_list
	dim as symtab_t ptr symtab

	if match( TK_FOR ) = 0 then
		expected( "FOR", lexer_stack.curr_lex->tk_str )
	end if

	index_expr = parse_addrof_deref( )

	if match( TK_CHAR_EQ ) = 0 then
		expected( "'='", lexer_stack.curr_lex->tk_str )
	end if

	from_expr = parse_expression( )

	if match( TK_TO ) = 0 then
		expected( "TO", lexer_stack.curr_lex->tk_str )
	end if

	to_expr = parse_expression( )

	if match( TK_EOL ) = 0 then
		expected( "end of line", lexer_stack.curr_lex->tk_str )
	end if

	symstack_push( )

	for_stmt_list = parse_stmt_list( )

	symtab = symstack_pop( )

	if match( TK_NEXT ) = 0 then
		expected( "NEXT", lexer_stack.curr_lex->tk_str )
	end if

	read_token( ) ' Dump ident

	if match( TK_EOL ) = 0 then
		expected( "end of line", lexer_stack.curr_lex->tk_str )
	end if

	return tree_node_for( index_expr, from_expr, to_expr, for_stmt_list, symtab )

end function
