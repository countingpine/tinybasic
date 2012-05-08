'::::::::
declare function parse_expression _
	( _
	) as node_t ptr

'::::::::
function parse_lvalue _
	( _
		byval dt as datatype_t ptr _
	) as string

	dim as string s
	dim as node_t ptr expr
	dim as sym_t ptr sym
	dim as sym_t ptr oldsym
	'dim as node_t ptr proc_expr(0 to 15)
	'dim as integer proc_expr_count
	'dim as integer is_proc

	do
		'proc_expr_count = 0
		sym = sym_find( tk_str, oldsym )
		dt->sym = sym
		s += tk_str
		read_token( )
		if tk_str = ":" then
			sym_add_label( s )
			exit do
		end if
		if sym = 0 then
			print "!! " & s
			expected( "symbol/member", tk_str )
		end if
		if tk_str = "(" then
			if sym->typ = SYM_ARRAY then
				s += "["
			else
				s += "("
				dt->is_proc = -1
			end if
			read_token( )
			while (tk_str <> ")") and (tk_typ <> TK_EOF)
				expr = parse_expression( )
				s += expr_to_str( expr )
				dt->expr(dt->expr_cnt) = expr
				dt->expr_cnt += 1
				if tk_str = "," then
					s += ", "
					read_token( )
				else
					exit while
				end if
			wend
			if match_str( ")" ) = 0 then
				expected( "')'", tk_str )
			end if
			if sym->typ = SYM_ARRAY then
				s += "]"
			else
				s += ")"
			end if
		elseif tk_str = "[" then
			s += "["
			read_token( )
			expr = parse_expression( )
			s += expr_to_str( expr )
			if match_str( "]" ) = 0 then
				expected( "']'", tk_str )
			end if
			s += "]"
		end if
		if tk_str = "." then
			s += "."
			read_token( )
		elseif tk_str = "->" then
			s += "->"
			read_token( )
		else
			exit do
		end if
		oldsym = sym
	loop

	if sym then
		dim as datatype_t ptr _dt = sym->dt
		if sym->typ <> SYM_DATATYPE then
			if _dt then
				dt->s = _dt->s
				dt->ptr_cnt = _dt->ptr_cnt
			end if
		end if
	end if

	function = s

end function

'::::::::
function parse_atom _
	( _
	) as node_t ptr

	if tk_typ = TK_LITINT then
		function = tree_node_litint( tk_str )
		read_token( )
	elseif tk_typ = TK_LITSTR then
		function = tree_node_litstr( tk_str )
		read_token( )
	elseif tk_typ = TK_IDENT then
		dim as datatype_t ptr _dt = callocate( sizeof( datatype_t ) )
		dim as string lvalue = parse_lvalue( _dt )
		function = tree_node_lvalue( lvalue, _dt )
	elseif tk_str = "(" then
		read_token( )
		function = parse_expression( )
		if match_str( ")" ) = 0 then
			expected( "')'", tk_str )
		end if
	elseif tk_str = "-" then
		read_token( )
		function = tree_node_uop( "-", parse_expression( ) )
	elseif tk_str = "@" then
		read_token( )
		function = tree_node_uop( "@", parse_expression( ) )
	else
		expected( "atom", tk_str )
	end if

end function

'::::::::
function parse_muldiv _
	( _
	) as node_t ptr

	dim as node_t ptr node

	node = parse_atom( )
	while (tk_str = "*") or (tk_str = "/")
		dim as string bop = tk_str
		read_token( )
		node = tree_node_bop( bop, node, parse_atom( ) )
	wend

	function = node

end function

'::::::::
function parse_addsub _
	( _
	) as node_t ptr

	dim as node_t ptr node

	node = parse_muldiv( )
	while (tk_str = "+") or (tk_str = "-")
		dim as string bop = tk_str
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
	while (tk_str = "&")
		dim as string bop = tk_str
		read_token( )
		node = tree_node_bop( bop, node, parse_addsub( ) )
		if node->l->dt.s <> node->r->dt.s then
			if (node->l->dt.s = "FBSTRING") and (node->r->dt.s = "integer") then
				node->r = tree_node_convert( node->r, "FBSTRING" )
			elseif (node->r->dt.s = "FBSTRING") and (node->l->dt.s = "integer") then
				node->l = tree_node_convert( node->l, "FBSTRING" )
			end if
		end if
	wend

	function = node

end function

'::::::::
function parse_rel _
	( _
	) as node_t ptr

	dim as node_t ptr node

	node = parse_strconcat( )
	while (tk_str = "=") or (tk_str = "<>") _
	   or (tk_str = "<") or (tk_str = "<=") _
	   or (tk_str = ">") or (tk_str = ">=")
		dim as string bop = tk_str
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
	while (lcase( tk_str ) = "and")
		dim as string bop = tk_str
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
	while (lcase( tk_str ) = "or")
		dim as string bop = tk_str
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

