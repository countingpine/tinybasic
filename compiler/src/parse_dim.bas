'::::::::
function list_dim_item _
	( _
		byval item  as dim_item_t, _
		byval _next as list_dim_item_t ptr _
	) as list_dim_item_t ptr

	dim as list_dim_item_t ptr list = callocate( sizeof( list_dim_item_t ) )

	list->item = item
	list->_next = _next

	function = list

end function

'::::::::
' <dim_array_bounds> ::= "(" <expression> "TO" <expression> ")"
'::::::::
function parse_dim_array_bounds _
	( _
	) as dim_array_bounds_t

	dim as dim_array_bounds_t result

	' "("
	if match( TK_CHAR_LPAREN ) = 0 then
		expected( "'('", lexer_stack.curr_lex->tk_str )
	end if

	' <expression>
	result.lower_expr = parse_expression( )

	' "TO"
	if match( TK_TO ) = 0 then
		expected( "TO", lexer_stack.curr_lex->tk_str )
	end if

	' <expression>
	result.upper_expr = parse_expression( )

	' ")"
	if match( TK_CHAR_RPAREN ) = 0 then
		expected( "')'", lexer_stack.curr_lex->tk_str )
	end if

	function = result

end function

'::::::::
' <dim_item> ::= <ident> [<dim_array_bounds>] ["=" <expression>]
'::::::::
function parse_dim_item _
	( _
		byval dt as datatype_t ptr _
	) as dim_item_t

	dim as dim_item_t result

	result.dt = new_datatype( dt->_dt, dt->_ptr_cnt, dt->_sym )

	' <ident>
	result.__ident = lexer_stack.curr_lex->tk_str
	read_token( ) ' dump ident

	' [<dim_array_bounds>]
	if lexer_stack.curr_lex->tk_typ = TK_CHAR_LPAREN then
		result.is_array = -1
		result.bounds = parse_dim_array_bounds( )
	end if

	' ["=" <expression>]
	if match( TK_CHAR_EQ ) then
		result.init_expr = parse_expression( )
	end if

	function = result

end function

'::::::::
' <dim_stmt> ::= "DIM" ["SHARED"] "AS" <datatype> <dim_item> {, <dim_item>}* <eol>
'::::::::
function parse_dim_stmt _
	( _
	) as dim_stmt_t

	dim as dim_stmt_t result
	dim as datatype_t ptr dt

	' "DIM"
	if match( TK_DIM ) = 0 then
		expected( "DIM", lexer_stack.curr_lex->tk_str )
	end if

	' ["SHARED"]
	if match( TK_SHARED ) then
		result.is_shared = -1
	end if

	' "AS"
	if match( TK_AS ) = 0 then
		expected( "AS", lexer_stack.curr_lex->tk_str )
	end if

	' <datatype>
	dt = parse_datatype( )

	' <dim_item>
	result.item_list = list_dim_item( parse_dim_item( dt ), 0 )

	' {, <dim_item>}*
	dim as list_dim_item_t ptr tmp = result.item_list
	while match( TK_CHAR_COMMA )
		tmp->_next = list_dim_item( parse_dim_item( dt ), 0 )
		tmp = tmp->_next
	wend

	' <eol>
	if match( TK_EOL ) = 0 then
		expected( "end of line", lexer_stack.curr_lex->tk_str )
	end if

	free( dt )

	function = result

end function

'::::::::
function parse_dim _
	( _
	) as node_t ptr

	dim as dim_stmt_t dim_stmt
	dim as list_dim_item_t ptr list

	dim_stmt = parse_dim_stmt( )

	' Now check the dim statement, and create the symbols
	list = dim_stmt.item_list
	while list
		dim as integer array_size

		' Is is an array?
		if list->item.is_array then
			' Check that the bounds expressions are constant
			if list->item.bounds.lower_expr->typ <> NODE_LITINT then
				die( "Lower array bounds in DIM must be constant" )
			end if

			if list->item.bounds.upper_expr->typ <> NODE_LITINT then
				die( "Upper array bounds in DIM must be constant" )
			end if
			
			if val( list->item.bounds.lower_expr->_i ) <> 0 then
				die( "Lower array bound must be 0" )
			end if

			array_size = val( list->item.bounds.upper_expr->_i ) + 1
		end if

		' No symbol with this name?
		if sym_exists( list->item.__ident ) then
			' If there is already a symbol with this name, it's only allowed
			' if that prior one was an EXTERN decl
			if sym_find( list->item.__ident, 0 )->typ <> SYM_EXTERN_VAR then
				die( "symbol '" & list->item.__ident & "' already exists" )
			end if
		end if

		' Allocate the symbol
		list->item.sym = sym_add_dim( list->item.__ident, list->item.__ident, new_datatype( list->item.dt->_dt, list->item.dt->_ptr_cnt, list->item.dt->_sym ), list->item.is_array, array_size, dim_stmt.is_shared )

		' If there is an initializer, then provide implicit conversion if needed
		if list->item.init_expr then
			' a dim expression has an assign, l = r
			dim as datatype_t ptr ldt = list->item.dt
			dim as datatype_t ptr rdt = @list->item.init_expr->dt

			' are the datatypes different?
			if ((ldt->_dt = rdt->_dt) and (ldt->_ptr_cnt = rdt->_ptr_cnt) and (ldt->_sym = rdt->_sym)) = 0 then
				' If so, convert the rhs, ie the initializer
				list->item.init_expr = tree_node_convert( list->item.init_expr, ldt )
			end if
		end if

		' Onto the next item
		list = list->_next
	wend

	dim as node_t ptr node = callocate( sizeof( node_t ) )

	node->typ = NODE_DIM
	node->dim_stmt = callocate( sizeof( dim_stmt_t ) )
	*node->dim_stmt = dim_stmt

	function = node

end function
