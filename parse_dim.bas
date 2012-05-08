'::::::::
function parse_dim _
	( _
	) as node_t ptr

	dim as datatype_t ptr dt
	dim as string ident
	dim as node_t ptr expr
	dim as integer is_array
	dim as integer array_size
	dim as integer is_shared
	dim as sym_t ptr sym

	if match_str( "shared" ) then
		is_shared = -1
	end if

	if match_str( "as" ) = 0 then
		expected( "as", tk_str )
	end if

	dt = parse_datatype( )

	ident = tk_str
	read_token( ) ' dump ident

	' Will it be an array?
	if match_str( "(" ) then
		is_array = -1
		if match_str( "0" ) = 0 then
			expected( "0", tk_str )
		end if
		if match_str( "to" ) = 0 then
			expected( "TO", tk_str )
		end if
		array_size = val(tk_str) + 1
		read_token( ) ' dump ubound
		if match_str( ")" ) = 0 then
			expected( "')'", tk_str )
		end if
	end if

	' Is there an initializer?
	if match_str( "=" ) then
		expr = parse_expression( )
	end if

	if match( TK_EOL ) = 0 then
		expected( "end of line", tk_str )
	end if

	' No symbol with this name?
	if sym_exists( ident ) then
		print "Symbol '" & ident & "' already exists!"
		exit_( 1 )
	end if

	' Allocate the symbol
	sym = sym_add_dim( ident, dt, is_array, array_size, is_shared )

	dim as node_t ptr node = callocate( sizeof( node_t ) )

	node->typ = NODE_DIM
	node->dim_sym = sym
	node->dim_dt = dt
	node->dim_init_expr = expr

	function = node

end function

