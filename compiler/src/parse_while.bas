'::::::::
function parse_while _
	( _
	) as node_t ptr

	dim as node_t ptr expr
	dim as node_t ptr stmt_list

	expr = parse_expression( )

	if match( TK_EOL ) = 0 then
		expected( "end of line", lexer_stack.curr_lex->tk_str )
	end if

	symstack_push( )

	stmt_list = parse_stmt_list( )
	
	function = tree_node_while( expr, stmt_list, symstack_pop( ) )

end function
