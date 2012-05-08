type proc_header_t
	ident   as string
	dt      as datatype_t ptr
	pi      as param_info_t ptr
	p_count as integer
end type

function parse_proc_header _
	( _
	) as proc_header_t ptr

	dim as proc_header_t ptr pl

	pl = callocate( sizeof( proc_header_t ) )
	pl->pi = callocate( sizeof( param_info_t ) * 16 )

	pl->ident = lexer_stack.curr_lex->tk_str
	read_token( )

	if match( TK_CHAR_LPAREN ) = 0 then
		expected( "'('", lexer_stack.curr_lex->tk_str )
	end if

	while (lexer_stack.curr_lex->tk_typ <> TK_CHAR_RPAREN) _
	  and (lexer_stack.curr_lex->tk_typ <> TK_EOF)
		dim as param_info_t ptr pi = @pl->pi[pl->p_count]

		if match( TK_BYREF ) then
			pi->is_byref = -1
		elseif match( TK_BYVAL ) then
			pi->is_byval = -1
		else
			expected( "BYVAL/BYREF", lexer_stack.curr_lex->tk_str )
		end if

		pi->ident = lexer_stack.curr_lex->tk_str
		read_token( )

		if match( TK_AS ) = 0 then
			expected( "AS", lexer_stack.curr_lex->tk_str )
		end if

		pi->dt = parse_datatype( )

		pi->sym = sym_add_dim( pi->ident, pi->ident, new_datatype( pi->dt->_dt, pi->dt->_ptr_cnt, pi->dt->_sym ), 0, 0, 0 )

		pl->p_count += 1

		if match( TK_CHAR_COMMA ) = 0 then
			exit while
		end if
	wend

	if match( TK_CHAR_RPAREN ) = 0 then
		expected( "')'", lexer_stack.curr_lex->tk_str )
	end if

	if match( TK_AS ) then
		pl->dt = parse_datatype( )
	else
		pl->dt = 0
	end if

	if match( TK_EOL ) = 0 then
		expected( "end of line", lexer_stack.curr_lex->tk_str )
	end if

	function = pl

end function

'::::::::
function parse_proc _
	( _
		byval is_func as integer, _
		byval is_decl as integer _
	) as node_t ptr

	dim as proc_header_t ptr pl
	dim as string            s
	dim as integer           i
	dim as sym_t ptr         sym
	dim as node_t ptr        stmt_list

	symstack_push( )

	pl = parse_proc_header( )

	'print "/* " & pl->ident & " " & pl->p_count & " */"
	sym = sym_add_proc( pl->ident, pl->ident, pl->dt, pl->pi, pl->p_count )

	if is_decl = 0 then
		stmt_list = parse_stmt_list( )
		function = tree_node_proc( sym, stmt_list, symstack_pop( ) )
	else
		function = tree_node_proc_decl( sym, symstack_pop( ) )
	end if

	pl->ident = ""
	'free(pl->pi)
	free(pl)

end function
