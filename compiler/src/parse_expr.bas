'::::::::
declare function parse_expression _
	( _
	) as node_t ptr

'::::::::
function parse_atom_proccall _
	( _
		byval old_sym as sym_t ptr _
	) as node_t ptr

	dim as string ident
	dim as sym_t ptr sym
	dim as node_t ptr expr(0 to 15)
	dim as integer expr_cnt

	ident = lexer_stack.curr_lex->tk_str
	read_token( )

	sym = sym_find( ident, old_sym )

	if match( TK_CHAR_LPAREN ) then
		while (lexer_stack.curr_lex->tk_typ <> TK_CHAR_RPAREN) _
		  and (lexer_stack.curr_lex->tk_typ <> TK_EOF)
			expr(expr_cnt) = parse_expression( )
			expr_cnt += 1
			if match( TK_CHAR_COMMA ) = 0 then
				exit while
			end if
		wend
		if match( TK_CHAR_RPAREN ) = 0 then
			expected( "')'", lexer_stack.curr_lex->tk_str )
		end if
		function = tree_node_proccall( sym, @expr(0), expr_cnt )
	else
		expected( "'('", lexer_stack.curr_lex->tk_str )
	end if

end function

'::::::::
function parse_atom_array_access _
	( _
		byval old_sym as sym_t ptr _
	) as node_t ptr

	dim as string ident
	dim as sym_t ptr sym
	dim as node_t ptr expr(0 to 15)
	dim as integer expr_cnt

	ident = lexer_stack.curr_lex->tk_str
	read_token( )

	sym = sym_find( ident, old_sym )

	if match( TK_CHAR_LPAREN ) then
		while (lexer_stack.curr_lex->tk_typ <> TK_CHAR_RPAREN) _
		  and (lexer_stack.curr_lex->tk_typ <> TK_EOF)
			expr(expr_cnt) = parse_expression( )
			expr_cnt += 1
			if lexer_stack.curr_lex->tk_str = "," then
				read_token( )
			else
				exit while
			end if
		wend

		if match( TK_CHAR_RPAREN ) = 0 then
			expected( "')'", lexer_stack.curr_lex->tk_str )
		end if

		function = tree_node_array_access( sym, @expr(0), expr_cnt )
	else
		expected( "'('", lexer_stack.curr_lex->tk_str )
	end if

end function

'::::::::
function parse_atom_ptr_array_access _
	( _
		byval sym as sym_t ptr, _
		byval old_sym as sym_t ptr _
	) as node_t ptr

	dim as node_t ptr expr(0 to 15)
	dim as integer expr_cnt

	if match( TK_CHAR_LSBRAC ) = 0 then
		expected( "'['", lexer_stack.curr_lex->tk_str )
	end if

	while (lexer_stack.curr_lex->tk_typ <> TK_CHAR_RSBRAC) _
	  and (lexer_stack.curr_lex->tk_typ <> TK_EOF)
		expr(expr_cnt) = parse_expression( )
		expr_cnt += 1

		if lexer_stack.curr_lex->tk_str = "," then
			read_token( )
		else
			exit while
		end if
	wend

	if match( TK_CHAR_RSBRAC ) = 0 then
		expected( "']'", lexer_stack.curr_lex->tk_str )
	end if

	function = tree_node_ptr_array_access( sym, @expr(0), expr_cnt )

end function

'::::::::
function parse_atom _
	( _
		byval old_sym as sym_t ptr _
	) as node_t ptr

	if lexer_stack.curr_lex->tk_typ = TK_LITINT then
		function = tree_node_litint( lexer_stack.curr_lex->tk_str )
		read_token( )
	elseif lexer_stack.curr_lex->tk_typ = TK_LITSTR then
		function = tree_node_litstr( lexer_stack.curr_lex->tk_str )
		read_token( )
	elseif lexer_stack.curr_lex->tk_typ = TK_IDENT then
		if old_sym then
			if old_sym->typ = SYM_DATATYPE_FWD then
				old_sym = sym_find( old_sym->_real, 0 )
			end if
		end if

		dim as sym_t ptr sym = sym_find( lexer_stack.curr_lex->tk_str, old_sym )
		dim as string ident

		if sym = 0 then
			' Label?
			ident = lexer_stack.curr_lex->tk_str
			read_token( )

			if match( TK_CHAR_COLON ) then
				sym = sym_add_label( ident )
				return tree_node_label( sym )
			end if

			expected( "identifier", "'" & ident & "'" )
		end if

		if sym->is_proc then
			function = parse_atom_proccall( old_sym )
		elseif sym->is_array then
			function = parse_atom_array_access( old_sym )
		elseif (sym->typ = SYM_VAR) or (sym->typ = SYM_EXTERN_VAR) then
			read_token( ) ' dump ident
			if lexer_stack.curr_lex->tk_typ = TK_CHAR_LSBRAC then
				return parse_atom_ptr_array_access( sym, old_sym )
			end if
			function = tree_node_var( sym )
		elseif sym->typ = SYM_LABEL then
			read_token( ) ' dump ident
			function = tree_node_label( sym )
		elseif sym->typ = SYM_DATATYPE then
			read_token( ) ' dump ident
			function = tree_node_type( sym )
		elseif sym->typ = SYM_ENUM_MEMBER then
			read_token( ) ' dump ident
			function = tree_node_enum_val( sym )
		else
			die( "Unhandled expr" )
		end if
	elseif match( TK_CHAR_LPAREN ) then
		function = parse_expression( )
		if match( TK_CHAR_RPAREN ) = 0 then
			expected( "')'", lexer_stack.curr_lex->tk_str )
		end if
	else
		expected( "atom", lexer_stack.curr_lex->tk_str )
	end if

end function

'::::::::
function parse_access _
	( _
	) as node_t ptr

	dim as node_t ptr node
	dim as node_t ptr r

	node = parse_atom( 0 )
	r = node
	while (lexer_stack.curr_lex->tk_typ = TK_ARROW) _
	   or (lexer_stack.curr_lex->tk_typ = TK_CHAR_DOT)
		dim as string bop = lexer_stack.curr_lex->tk_str
		read_token( )
		dim as sym_t ptr sym
		if r->sym->dt->_dt = DT_TYPE then
			sym = r->sym->dt->_sym
		else
			print "NOT A TYPE!!"
			exit_( 1 )
		end if
		node = tree_node_bop( bop, node, parse_atom( sym ) )
		node->dt = node->r->dt
		r = node->r
	wend

	function = node

end function

'::::::::
function parse_addrof_deref _
	( _
	) as node_t ptr

	dim as node_t ptr node
	dim as string uop

	if (lexer_stack.curr_lex->tk_typ = TK_CHAR_AT) or (lexer_stack.curr_lex->tk_typ = TK_CHAR_ASTERISK) then
		uop = lexer_stack.curr_lex->tk_str
		read_token( )
		node = tree_node_uop( uop, parse_addrof_deref( ) )
	else
		node = parse_access( )
	end if

	function = node

end function

'::::::::
function parse_negate _
	( _
	) as node_t ptr

	if match( TK_CHAR_MINUS ) then
		function = tree_node_uop( "-", parse_negate( ) )
	else
		function = parse_addrof_deref( )
	end if

end function

'::::::::
function parse_muldiv _
	( _
	) as node_t ptr

	dim as node_t ptr node

	node = parse_negate( )
	while (lexer_stack.curr_lex->tk_typ = TK_CHAR_ASTERISK) _
	   or (lexer_stack.curr_lex->tk_str = "/")
		dim as string bop = lexer_stack.curr_lex->tk_str
		read_token( )
		node = tree_node_bop( bop, node, parse_negate( ) )
	wend

	function = node

end function

'::::::::
function parse_addsub _
	( _
	) as node_t ptr

	dim as node_t ptr node

	node = parse_muldiv( )
	while (lexer_stack.curr_lex->tk_typ = TK_CHAR_PLUS) _
	   or (lexer_stack.curr_lex->tk_typ = TK_CHAR_MINUS)
		dim as string bop = lexer_stack.curr_lex->tk_str
		read_token( )
		node = tree_node_bop( bop, node, parse_muldiv( ) )
	wend

	function = node

end function

'::::::::
function parse_strconcat _
	( _
	) as node_t ptr

	dim as node_t ptr node

	node = parse_addsub( )
	while lexer_stack.curr_lex->tk_str = "&"
		dim as string bop = lexer_stack.curr_lex->tk_str
		read_token( )
		node = tree_node_bop( bop, node, parse_addsub( ) )
		dim as datatype_t ptr dt = callocate( sizeof( datatype_t ) )
		dt->_dt = DT_STRING
		node->dt = *dt
		if node->l->dt._dt <> DT_STRING then
			node->l = tree_node_convert( node->l, dt )
		end if
		if node->r->dt._dt <> DT_STRING then
			node->r = tree_node_convert( node->r, dt )
		end if
		free(dt)
	wend

	function = node

end function

'::::::::
function parse_rel _
	( _
	) as node_t ptr

	dim as node_t ptr node

	node = parse_strconcat( )
	while (lexer_stack.curr_lex->tk_typ = TK_CHAR_EQ) _
	   or (lexer_stack.curr_lex->tk_typ = TK_CHAR_LT) _
	   or (lexer_stack.curr_lex->tk_typ = TK_CHAR_GT) _
	   or (lexer_stack.curr_lex->tk_typ = TK_NE) _
	   or (lexer_stack.curr_lex->tk_typ = TK_LE) _
	   or (lexer_stack.curr_lex->tk_typ = TK_GE)
		dim as string bop = lexer_stack.curr_lex->tk_str
		read_token( )
		node = tree_node_bop( bop, node, parse_strconcat( ) )
	wend

	function = node

end function

'::::::::
function parse_and _
	( _
	) as node_t ptr

	dim as node_t ptr node

	node = parse_rel( )
	while lexer_stack.curr_lex->tk_typ = TK_AND
		dim as string bop = lexer_stack.curr_lex->tk_str
		read_token( )
		node = tree_node_bop( bop, node, parse_rel( ) )
	wend

	function = node

end function

'::::::::
function parse_or _
	( _
	) as node_t ptr

	dim as node_t ptr node

	node = parse_and( )
	while lexer_stack.curr_lex->tk_typ = TK_OR
		dim as string bop = lexer_stack.curr_lex->tk_str
		read_token( )
		node = tree_node_bop( bop, node, parse_and( ) )
	wend

	function = node

end function

'::::::::
function parse_expression _
	( _
	) as node_t ptr

	function = parse_or( )

end function
