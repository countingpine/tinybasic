'::::::::
function parse_elseif _
	( _
	) as node_t ptr

	dim as node_t ptr expr
	dim as node_t ptr stmt_list

	expr = parse_expression( )

	if match( TK_THEN ) = 0 then
		expected( "THEN", lexer_stack.curr_lex->tk_str )
	end if

	if match( TK_EOL ) = 0 then
		expected( "end of line", lexer_stack.curr_lex->tk_str )
	end if

	symstack_push( )

	stmt_list = parse_stmt_list( )

	function = tree_node_elseif( expr, stmt_list, symstack_pop( ) )

end function

'::::::::
function parse_elseif_list _
	( _
	) as node_t ptr

	dim as node_t ptr elseif_list

	while match( TK_ELSEIF )
		elseif_list = tree_node_elseif_list( elseif_list, parse_elseif( ) )
	wend

	function = elseif_list

end function

'::::::::
function parse_else _
	( _
	) as node_t ptr

	dim as node_t ptr stmt_list

	if match( TK_ELSE ) = 0 then
		return 0
	end if

	if match( TK_EOL ) = 0 then
		expected( "end of line", lexer_stack.curr_lex->tk_str )
	end if

	symstack_push( )

	stmt_list = parse_stmt_list( )

	function = tree_node_else( stmt_list, symstack_pop( ) )

end function

'::::::::
function parse_if _
	( _
	) as node_t ptr

	dim as node_t ptr if_expr
	dim as node_t ptr if_stmt_list
	dim as node_t ptr elseif_list
	dim as node_t ptr else_node
	dim as symtab_t ptr symtab

	if_expr = parse_expression( )

	if match( TK_THEN ) = 0 then
		expected( "THEN", lexer_stack.curr_lex->tk_str )
	end if

	if match( TK_EOL ) = 0 then
		expected( "end of line", lexer_stack.curr_lex->tk_str )
	end if

	symstack_push( )

	if_stmt_list = parse_stmt_list( )

	symtab = symstack_pop( )

	if (lexer_stack.curr_lex->old_tk_typ = TK_END) and (lexer_stack.curr_lex->tk_typ = TK_IF) then
		read_token( )
		if match( TK_EOL ) = 0 then
			expected( "end of line", lexer_stack.curr_lex->tk_str )
		end if
		return tree_node_if( if_expr, if_stmt_list, elseif_list, else_node, symtab )
	end if

	elseif_list = parse_elseif_list( )

	if (lexer_stack.curr_lex->old_tk_typ = TK_END) and (lexer_stack.curr_lex->tk_typ = TK_IF) then
		read_token( )
		if match( TK_EOL ) = 0 then
			expected( "end of line", lexer_stack.curr_lex->tk_str )
		end if
		return tree_node_if( if_expr, if_stmt_list, elseif_list, else_node, symtab )
	end if

	else_node = parse_else( )

	if (lexer_stack.curr_lex->old_tk_typ = TK_END) and (lexer_stack.curr_lex->tk_typ = TK_IF) then
		read_token( )
		if match( TK_EOL ) = 0 then
			expected( "end of line", lexer_stack.curr_lex->tk_str )
		end if
	else
		expected( "end if", lexer_stack.curr_lex->tk_str )
	end if

	'print if_expr & "-" & if_stmt_list & "-" & elseif_list & "-" & else_stmt_list
	function = tree_node_if( if_expr, if_stmt_list, elseif_list, else_node, symtab )

end function
