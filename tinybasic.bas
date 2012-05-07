#include once "crt.bi"
#undef rem
#define rem

dim shared as integer CHAR_DBLQUOTE = 34

dim shared as integer SYM_BAD      = 0
dim shared as integer SYM_DATATYPE = 1
dim shared as integer SYM_VAR      = 2
dim shared as integer SYM_PROC     = 3
dim shared as integer SYM_ARRAY    = 4
dim shared as integer SYM_LABEL    = 5

type sym_t
	typ as integer
	ident as string
	dt as any ptr
	is_array as integer
	is_shared as integer
	array_size as integer
	dt_parent as sym_t ptr
	members as sym_t ptr
	member_count as integer
end type

type datatype_t
	dt as sym_t ptr
	ptr_cnt as integer
	s as string
	sym as sym_t ptr
	expr(0 To 15) as any ptr
	expr_cnt as integer
	is_proc as integer
end type

type symtab_t
	syms(0 To 255) as sym_t
	sym_count as integer
end type

type symstack_t
	stk(0 To 15) as symtab_t
	stk_p as integer
end type

dim shared as integer NODE_BAD      = 0
dim shared as integer NODE_LITINT   = 1
dim shared as integer NODE_LITSTR   = 2
dim shared as integer NODE_BOP      = 3
dim shared as integer NODE_LVALUE   = 4
dim shared as integer NODE_UOP      = 5
dim shared as integer NODE_CONVERT  = 6
dim shared as integer NODE_PROCCALL = 7
dim shared as integer NODE_DIM      = 8
type node_t
	typ as integer
	s as string
	l as node_t ptr
	r as node_t ptr
	dt as datatype_t
	expr(0 To 15) as node_t ptr
	expr_cnt as integer
	sym as sym_t ptr

	dim_sym	as sym_t ptr
	dim_dt	 as datatype_t ptr
	dim_init_expr  as node_t ptr

end type

dim shared as integer TK_BAD     = 0
dim shared as integer TK_IDENT   = 256
dim shared as integer TK_LITINT  = 257
dim shared as integer TK_EOF     = 258
dim shared as integer TK_LITSTR  = 259
dim shared as integer TK_EOL     = 260
dim shared as integer TK_SELFADD = 261
dim shared as integer TK_NE      = 262
dim shared as integer TK_SELFSUB = 263
dim shared as integer TK_ARROW   = 264
dim shared as integer TK_GE      = 265

dim shared as integer look
dim shared as string  tk_str
dim shared as integer tk_typ

dim shared as symstack_t symstack

dim shared as integer indent

dim shared as integer curr_line

'*******************************************************************************
' Emit
'*******************************************************************************

'::::::::
sub emit_line _
	( _
		byref txt as string _
	)

	dim as integer i

	i = 0
	while i < indent
		print !"\t";
		i += 1
	wend

	print txt

end sub

'*******************************************************************************
' Error
'*******************************************************************************

'::::::::
sub expected _
	( _
		byref what  as string, _
		byref found as string _
	)

	print "Expected : " & what
	print "Found    : " & found
	print "Line     : " & curr_line
	exit_( 1 )

end sub

'*******************************************************************************
' Symbol table
'*******************************************************************************

'::::::::
sub symstack_push _
	( _
	)

	symstack.stk_p += 1
	symstack.stk(symstack.stk_p).sym_count = 0

end sub

'::::::::
sub symstack_pop _
	( _
	)

	symstack.stk_p -= 1

end sub

'::::::::
function sym_exists _
	( _
		byref ident as string _
	) as integer

	dim as integer i

	i = 0
	while i < symstack.stk(symstack.stk_p).sym_count
		if lcase( symstack.stk(symstack.stk_p).syms(i).ident ) = lcase( ident ) then
			Return -1
		end if
		i += 1
	wend

end function

'::::::::
function sym_find _
	( _
		byref ident as string, _
		byval sym as sym_t ptr _
	) as sym_t ptr

	dim as integer i
	dim as integer p = symstack.stk_p

	'print __FUNCTION__

	if sym = 0 then
		while p >= 0
			i = 0
			while i < symstack.stk(p).sym_count
				'print lcase( symstack.stk(p).syms(i).ident ) & " " & lcase( ident )
				if lcase( symstack.stk(p).syms(i).ident ) = lcase( ident ) then
					Return @symstack.stk(p).syms(i)
				end if
				i += 1
			wend
			p -= 1
		wend
	else
		dim as datatype_t ptr dt = sym->dt
		if dt then
			if dt->dt then
				i = 0
				while i < dt->dt->member_count
					'print lcase( dt->dt->members[i].ident ) & " " & lcase( ident )
					if lcase( dt->dt->members[i].ident ) = lcase( ident ) then
						Return @dt->dt->members[i]
					end if
					i += 1
				wend
			end if
		end if
		if sym->dt_parent then
			print "not yet done!"
		end if
	end if

end function

'::::::::
function sym_add_type _
	( _
		byref ident as string, _
		byval member_count as integer, _
		byval members as sym_t ptr _
	) as sym_t ptr

	dim as integer _pos = symstack.stk(0).sym_count

	symstack.stk(0).syms(_pos).typ = SYM_DATATYPE
	symstack.stk(0).syms(_pos).ident = ident
	symstack.stk(0).syms(_pos).member_count = member_count
	symstack.stk(0).syms(_pos).members = members

	function = @symstack.stk(0).syms(_pos)

	symstack.stk(0).sym_count += 1

end function

'::::::::
function sym_add_proc _
	( _
		byref ident as string _
	) as sym_t ptr

	dim as integer _pos = symstack.stk(0).sym_count

	symstack.stk(0).syms(_pos).typ = SYM_PROC
	symstack.stk(0).syms(_pos).ident = ident
	symstack.stk(0).syms(_pos).dt = callocate( sizeof( datatype_t ) )

	dim as datatype_t ptr dt = symstack.stk(0).syms(_pos).dt
	dt->s = "<PROC>"

	function = @symstack.stk(0).syms(_pos)

	symstack.stk(0).sym_count += 1

end function

'::::::::
function sym_add_dim _
	( _
		byref ident as string, _
		byval dt as datatype_t ptr, _
		byval is_array as integer, _
		byval array_size as integer, _
		byval is_shared as integer _
	) as sym_t ptr

	dim as integer _pos = symstack.stk(symstack.stk_p).sym_count
	dim as sym_t ptr dt_parent

	dt_parent = sym_find( dt->s, 0 )

	if is_array then
		symstack.stk(symstack.stk_p).syms(_pos).typ = SYM_ARRAY
	else
		symstack.stk(symstack.stk_p).syms(_pos).typ = SYM_VAR
	end if
	symstack.stk(symstack.stk_p).syms(_pos).ident = ident
	symstack.stk(symstack.stk_p).syms(_pos).dt = dt
	symstack.stk(symstack.stk_p).syms(_pos).is_array = is_array
	symstack.stk(symstack.stk_p).syms(_pos).array_size = array_size
	symstack.stk(symstack.stk_p).syms(_pos).is_shared = is_shared
	symstack.stk(symstack.stk_p).syms(_pos).dt_parent = dt_parent

	function = @symstack.stk(symstack.stk_p).syms(_pos)

	symstack.stk(symstack.stk_p).sym_count += 1

end function

'::::::::
sub sym_add_label _
	( _
		byref ident as string _
	)

	dim as integer _pos = symstack.stk(symstack.stk_p).sym_count

	symstack.stk(symstack.stk_p).syms(_pos).typ = SYM_LABEL
	symstack.stk(symstack.stk_p).syms(_pos).ident = ident

	symstack.stk(symstack.stk_p).sym_count += 1

end sub

'*******************************************************************************
' Lexer
'*******************************************************************************

'::::::::
sub read_char _
	( _
	)

	look = fgetc( stdin )

end sub

'::::::::
sub skip_white _
	( _
	)

	while (look = asc( " " )) or (look = asc( !"\t" ))
		read_char( )
	wend

end sub

'::::::::
function read_ident _
	( _
	) as string

	dim as string result

	while isalnum( look ) or (look = asc( "_" ))
		result += chr( look )
		read_char( )
	wend

	function = result

end function

'::::::::
function read_litint _
	( _
	) as string

	dim as string result

	while isdigit( look )
		result += chr( look )
		read_char( )
	wend

	function = result

end function

'::::::::
function read_litstr _
	( _
	) as string

	dim as string result

	if look <> CHAR_DBLQUOTE then
		print !"Expected '" & chr( CHAR_DBLQUOTE ) & "'"
		exit_( 1 )
	end if

	read_char( )

	while (look <> EOF_) and (look <> CHAR_DBLQUOTE)
		result += chr( look )
		read_char( )
	wend

	if look <> CHAR_DBLQUOTE then
		print !"Expected '" & chr( CHAR_DBLQUOTE ) & "'"
		exit_( 1 )
	end if

	read_char( )

	function = "str_temp(" & chr( CHAR_DBLQUOTE )& result & chr( CHAR_DBLQUOTE ) & ")"

end function

'::::::::
sub read_token _
	( _
	)

read_token_start:

	skip_white( )

	tk_str = "<<BAD>>"
	tk_typ = TK_BAD

	if look = EOF_ then
		tk_str = "<<EOF>>"
		tk_typ = TK_EOF
	elseif isalpha( look ) or (look = asc( "_" )) then
		tk_str = read_ident( )
		tk_typ = TK_IDENT
		if tk_str = "_" then
			while (look = 10) or (look = 13)
				read_char( )
			wend
			goto read_token_start
		end if
	elseif isdigit( look ) then
		tk_str = read_litint( )
		tk_typ = TK_LITINT
	elseif look = CHAR_DBLQUOTE then
		tk_str = read_litstr( )
		tk_typ = TK_LITSTR
	elseif look = asc( "!" ) then
		read_char( )
		tk_str = read_litstr( )
		tk_typ = TK_LITSTR
	elseif (look = 10) or (look = 13) then
		while (look = 10) or (look = 13) or (look = asc( " " )) or (look = asc( !"\t" ))
			read_char( )
		wend
		tk_str = "<<EOL>>"
		tk_typ = TK_EOL
	elseif (look = asc( "'" )) then
		while (look <> 10) and (look <> 13) and (look <> EOF_)
			read_char( )
		wend
		'while (look = 10) or (look = 13)
		'	read_char( )
		'wend
		goto read_token_start
	elseif look = asc( "+" ) then
		tk_str = chr( look )
		tk_typ = look
		read_char( )
		if look = asc( "=" ) then
			tk_str = "+="
			tk_typ = TK_SELFADD
			read_char( )
		end if
	elseif look = asc( "-" ) then
		tk_str = chr( look )
		tk_typ = look
		read_char( )
		if look = asc( "=" ) then
			tk_str = "-="
			tk_typ = TK_SELFSUB
			read_char( )
		elseif look = asc( ">" ) then
			tk_str = "->"
			tk_typ = TK_ARROW
			read_char( )
		end if
	elseif look = asc( "<" ) then
		tk_str = chr( look )
		tk_typ = look
		read_char( )
		if look = asc( ">" ) then
			tk_str = "<>"
			tk_typ = TK_NE
			read_char( )
		end if
	elseif look = asc( ">" ) then
		tk_str = chr( look )
		tk_typ = look
		read_char( )
		if look = asc( "=" ) then
			tk_str = ">="
			tk_typ = TK_GE
			read_char( )
		end if
	else
		tk_str = chr( look )
		tk_typ = look
		read_char( )
	end if

	'print " // " & tk_str

	''''print "<<" & tk_str & ">>"

end sub

'::::::::
function match _
	( _
		byval t as integer _
	) as integer

	if tk_typ = t then
		function = -1
		read_token( )
	else
		function = 0
	end if

end function

'::::::::
function match_str _
	( _
		byref s as string _
	) as integer

	if lcase( tk_str ) = lcase( s ) then
		function = -1
		read_token( )
	else
		function = 0
	end if

end function


'*******************************************************************************
' Tree
'*******************************************************************************

'::::::::
function tree_node_litint _
	( _
		byref s as string _
	) as node_t ptr

	dim as node_t ptr node = callocate( sizeof( node_t ) )

	node->typ = NODE_LITINT
	node->s   = s
	node->dt.s = "integer"

	function = node

end function

'::::::::
function tree_node_litstr _
	( _
		byref s as string _
	) as node_t ptr

	dim as node_t ptr node = callocate( sizeof( node_t ) )

	node->typ = NODE_LITSTR
	node->s   = s
	node->dt.s = "FBSTRING"

	function = node

end function

'::::::::
function tree_node_bop _
	( _
		byref bop as string, _
		byval l as node_t ptr, _
		byval r as node_t ptr _
	) as node_t ptr

	dim as node_t ptr node = callocate( sizeof( node_t ) )

	node->typ = NODE_BOP
	node->s   = bop
	node->l   = l
	node->r   = r

	if l->dt.s <> r->dt.s then
		if (l->dt.ptr_cnt > 0) and (r->dt.s = "integer") then

		else
			'print "filae"
			'print l->dt.s & " " & r->dt.s
			'end 1
		end if
	end if

	node->dt = l->dt

	function = node

end function

'::::::::
function tree_node_uop _
	( _
		byref uop as string, _
		byval l as node_t ptr _
	) as node_t ptr

	dim as node_t ptr node = callocate( sizeof( node_t ) )

	node->typ = NODE_UOP
	node->s   = uop
	node->l   = l

	node->dt = l->dt

	function = node

end function

'::::::::
function tree_node_convert _
	( _
		byval l as node_t ptr, _
		byval dt as string _
	) as node_t ptr

	dim as node_t ptr node = callocate( sizeof( node_t ) )

	node->typ = NODE_CONVERT
	node->l   = l
	node->dt.s = dt

	function = node

end function

'::::::::
function tree_node_proccall _
	( _
		byval sym as sym_t ptr, _
		byval expr as node_t ptr ptr, _
		byval expr_cnt as integer _
	) as node_t ptr

	dim as node_t ptr node = callocate( sizeof( node_t ) )

	node->typ = NODE_PROCCALL
	node->sym = sym
	node->expr_cnt = expr_cnt

	dim as integer i = 0
	while i < expr_cnt
		node->expr(i) = expr[i]
		i += 1
	wend

	function = node

end function

'::::::::
function tree_node_lvalue _
	( _
		byref s as string, _
		byval dt as datatype_t ptr _
	) as node_t ptr

	if dt->expr_cnt > 0 then
		if dt->is_proc then
			dim as node_t ptr ptr expr = @dt->expr(0)
			dim as node_t ptr node = tree_node_proccall( dt->sym, expr, dt->expr_cnt )
			Return node
		end if
	end if

	dim as node_t ptr node = callocate( sizeof( node_t ) )

	node->typ = NODE_LVALUE
	node->s   = s
	node->dt.s = dt->s
	node->dt.ptr_cnt = dt->ptr_cnt

	function = node

end function

'*******************************************************************************
' ???
'*******************************************************************************

'::::::::
function expr_to_str _
	( _
		byval node as node_t ptr _
	) as string

	if node->typ = NODE_BOP then
		dim as string bop
		dim as string l
		dim as string r
		bop = node->s
		l = expr_to_str( node->l )
		r = expr_to_str( node->r )
		if bop = "=" then
			bop = "=="
		elseif bop = "<>" then
			bop = "!="
		elseif lcase( bop ) = "and" then
			bop = "&"
		elseif lcase( bop ) = "or" then
			bop = "|"
		elseif bop = "&" then
			bop = "concat"
		end if
		if bop = "concat" then
			function = "concat(" & l & ", " & r & ")"
		else
			function = "(" & l & " " & bop & " " & r & ")"
		end if
	elseif node->typ = NODE_UOP then
		dim as string uop
		dim as string l
		uop = node->s
		l = expr_to_str( node->l )
		if uop = "@" then
			uop = "&"
		end if
		function = "(" & uop & " " & l & ")"
	elseif node->typ = NODE_CONVERT then
		dim as string l
		l = expr_to_str( node->l )
		if (node->dt.s) = "FBSTRING" and (node->l->dt.s = "integer") then
			function = "int_to_str(" & l & ")"
		else
			print "No convert known!"
			exit_( 1 )
		end if
	elseif node->typ = NODE_PROCCALL then
		dim as string s
		s = node->sym->ident & "("
		dim as integer i = 0
		while i < node->expr_cnt
			s += expr_to_str( node->expr(i) )
			i += 1
			if i < node->expr_cnt then
				s += ", "
			end if
		wend
		s += ")"
		function = s
	else
		function = node->s
	end if

end function

'::::::::
function get_dt_size _
	( _
		byval dt as datatype_t ptr _
	) as integer

	if dt->ptr_cnt > 0 then
		function = 4
	elseif dt->s = "integer" then
		function = 4
	else
		function = 0
	end if

end function

'*******************************************************************************
' Parser
'*******************************************************************************

'::::::::
Declare function parse_expression _
	( _
	) as node_t ptr

'::::::::
sub parse_pp_include _
	( _
	)

	match_str( "once" )

	'emit_line( "#include " & tk_str )

	if match( TK_LITSTR ) = 0 then
		expected( "literal string", tk_str )
	end if

	if match( TK_EOL ) = 0 then
		expected( "end of line", tk_str )
	end if

end sub

'::::::::
sub parse_pp_dummy _
	( _
	)

	while (tk_typ <> TK_EOL) and (tk_typ <> TK_EOF)
		read_token( )
	wend

	if match( TK_EOL ) = 0 then
		expected( "end of line", tk_str )
	end if

end sub

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

'::::::::
function parse_datatype _
	( _
	) as datatype_t ptr

	dim as datatype_t ptr datatype = callocate( sizeof( datatype_t ) )
	dim as string dt = tk_str
	dim as integer ptr_cnt
	dim as integer i
	dim as string result

	if lcase( dt ) = "string" then
		dt = "FBSTRING"
	elseif lcase( dt ) = "any" then
		dt = "void"
	elseif lcase( dt ) = "integer" then
		dt = "integer"
	elseif lcase( dt ) = "zstring" then
		dt = "zstring"
	end if

	read_token( )

	while lcase( tk_str ) = "ptr"
		ptr_cnt += 1
		read_token( )
	wend

	result = dt

	i = 0
	while i < ptr_cnt
		result += " *"
		i += 1
	wend       

	datatype->dt = sym_find( dt, 0 )
	datatype->ptr_cnt = ptr_cnt
	datatype->s = result

	function = datatype
       
end function

'::::::::
function dim_to_str _
	( _
		byval node as node_t ptr _
	) as string

	dim as string s
	dim as integer do_memset

	s = node->dim_dt->s & " " & node->dim_sym->ident

	if node->dim_sym->is_array then
		s += "[" & node->dim_sym->array_size & "]"
	end if

	if node->dim_init_expr = 0 then
		if node->dim_dt->s = "integer" then
			if node->dim_sym->is_array = 0 then
				node->dim_init_expr = tree_node_litint( "0" )
			else
				do_memset = -1
			end if
		elseif node->dim_dt->ptr_cnt > 0 then
			if node->dim_sym->is_array = 0 then
				node->dim_init_expr = tree_node_litint( "0" )
			else
				do_memset = -1
			end if
		end if
	end if

	if node->dim_init_expr then
		s += " = (" & node->dim_dt->s & ")" & expr_to_str( node->dim_init_expr )
	end if

	s += ";"

	if do_memset then
		dim as integer sz = get_dt_size( node->dim_dt ) * node->dim_sym->array_size
		if sz <> 0 then
			s += !"\nmemset(" & node->dim_sym->ident & ", 0, " & sz & ");"
		else
			s += !"\n/* memset(" & node->dim_sym->ident & ", 0, " & sz & "); */"
		end if
	end if

	function = s

end function

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

'::::::::
sub parse_sub _
	( _
		byval is_decl as integer _
	)

	dim as string ident
	dim as datatype_t ptr dt
	dim as string param_ident(0 To 255)
	dim as datatype_t ptr param_dt(0 To 255)
	dim as integer param_is_byval(0 To 255)
	dim as integer param_is_byref(0 To 255)
	dim as integer p_count
	dim as string s
	dim as integer i

	ident = tk_str
	read_token( )

	if match_str( "(" ) = 0 then
		expected( "'('", tk_str )
	end if

	symstack_push( )

	while (tk_str <> ")") and (tk_typ <> TK_EOF)
		if match_str( "byref" ) then
			param_is_byval(p_count) = 0
			param_is_byref(p_count) = -1
		elseif match_str( "byval" ) then
			param_is_byval(p_count) = -1
			param_is_byref(p_count) = 0
		else
			expected( "byval/byref", tk_str )
		end if
		param_ident(p_count) = tk_str
		read_token( )
		if match_str( "as" ) = 0 then
			expected( "as", tk_str )
		end if
		param_dt(p_count) = parse_datatype( )
		sym_add_dim( param_ident(p_count), param_dt(p_count), 0, 0, 0 )
		p_count += 1
		if tk_str = "," then
			read_token( )
		else
			exit while
		end if
	wend

	if match_str( ")" ) = 0 then
		expected( "')'", tk_str )
	end if

	if match_str( "as" ) then
		dt = parse_datatype( )
	else
		dt = 0
	end if

	if match( TK_EOL ) = 0 then
		expected( "end of line", tk_str )
	end if

	emit_line( "/*::::::::*/" )
	if dt then
		s = dt->s & " "
	else
		s = "void "
	end if

	s += ident & "("

	i = 0
	while i < p_count
		if param_is_byref(i) then
			's += "const "
		end if
		s += param_dt(i)->s
		if param_is_byref(i) then
			s += "&"
		end if
		s += " " & param_ident(i)
		i += 1
		if i <> p_count then
			s += ", "
		end if
	wend
       
	s += ")"

	if is_decl then
		emit_line( s & ";" )
	else
		emit_line( s )
		emit_line( "{" )
		indent += 1
		if dt then
			emit_line( dt->s & " func$result = 0;" )
		end if
	end if

	sym_add_proc( ident )

end sub

'::::::::
function parse_while _
	( _
	) as node_t ptr

	function = parse_expression( )

	if match( TK_EOL ) = 0 then
		expected( "end of line", tk_str )
	end if

end function

'::::::::
function parse_if _
	( _
	) as node_t ptr

	function = parse_expression( )

	if match_str( "then" ) = 0 then
		expected( "THEN", tk_str )
	end if

	if match( TK_EOL ) = 0 then
		expected( "end of line", tk_str )
	end if

end function

'::::::::
sub parse_type _
	( _
	)

	dim as string  ident
	dim as integer member_count
	dim as sym_t ptr members = callocate( sizeof( sym_t ) * 256 )
	dim as integer i

	ident = tk_str
	read_token( ) ' dump identifier

	if match( TK_EOL ) = 0 then
		expected( "end of line", tk_str )
	end if

	while (lcase( tk_str ) <> "end") and (tk_typ <> TK_EOF)
		members[member_count].ident = tk_str
		read_token( ) ' dump identifier
		if match_str( "(" ) then
			members[member_count].typ = SYM_ARRAY
			if match_str( "0" ) = 0 then
				expected( "0", tk_str )
			end if
			if match_str( "to" ) = 0 then
				expected( "TO", tk_str )
			end if
			members[member_count].array_size = val(tk_str) + 1
			read_token( )
			if match_str( ")" ) = 0 then
				expected( "')'", tk_str )
			end if
		else
			members[member_count].typ = SYM_VAR
		end if
		if match_str( "as" ) = 0 then
			expected( "as", tk_str )
		end if
		members[member_count].dt = parse_datatype( )
		if match( TK_EOL ) = 0 then
			expected( "end of line", tk_str )
		end if
		member_count += 1
	wend

	if match_str( "end" ) = 0 then
		expected( "END", tk_str )
	end if

	if match_str( "type" ) = 0 then
		expected( "TYPE", tk_str )
	end if

	if match( TK_EOL ) = 0 then
		expected( "end of line", tk_str )
	end if

	if sym_exists( ident ) then
		print "Symbol '" & ident & "' already exists!"
		exit_( 1 )
	end if

	dim as sym_t ptr sym

	sym = sym_add_type( ident, member_count, members )

	emit_line( "/*::::::::*/" )
	emit_line( "struct " & ident & " {" )
	indent += 1
	i = 0
	while i < member_count
		dim as string s
		dim as datatype_t ptr dt = members[i].dt
		if dt->dt = 0 then
			dt->dt = sym
			dim as datatype_t ptr tmp = sym->members[i].dt
			tmp->dt->dt = sym
		end if
		s = dt->s & " " & members[i].ident
		if members[i].typ = SYM_ARRAY then
			s += "[" & members[i].array_size & "]"
		end if
		emit_line( s & ";" )
		i += 1
	wend
	indent -= 1
	emit_line( "};" )
	emit_line( "" )

end sub

'::::::::
sub parse_file _
	( _
	)

	moop:
	while tk_typ <> TK_EOF
		if tk_str = "#" then
			read_token( )
			if lcase( tk_str ) = "include" then
				read_token( )
				parse_pp_include( )
			elseif lcase( tk_str ) = "undef" then
				parse_pp_dummy( )
			elseif lcase( tk_str ) = "define" then
				parse_pp_dummy( )
			else
				print "Expected preprocessor command"
				exit_( 1 )
			end if
		elseif lcase( tk_str ) = "rem" then
			parse_pp_dummy( )
		elseif match_str( "dim" ) then
			dim as node_t ptr node
			dim as string s
			node = parse_dim( )
			s = dim_to_str( node )
			emit_line( s )
		elseif lcase( tk_str ) = "type" then
			read_token( )
			parse_type( )
		elseif (lcase( tk_str ) = "declare") then
			read_token( )
			read_token( )
			parse_sub( -1 )
		elseif (lcase( tk_str ) = "sub") or lcase( tk_str ) = "function" then
			dim as integer is_func = lcase( tk_str ) = "function"
			dim as node_t ptr expr
			read_token( )
			if is_func then
				if tk_str = "=" then
					if match_str( "=" ) = 0 then
						expected( "'='", tk_str )
					end if
					expr = parse_expression( )
					if match( TK_EOL ) = 0 then
						expected( "end of line", tk_str )
					end if
					emit_line( "func$result = " & expr_to_str( expr ) & ";" )
				else
					parse_sub( 0 )
				end if
			else
				parse_sub( 0 )
			end if
		elseif lcase( tk_str ) = "print" then
			dim as node_t ptr expr
			dim as integer has_semi
			read_token( )
			expr = parse_expression( )
			has_semi = match_str( ";" )
			if match( TK_EOL ) = 0 then
				expected( "end of line", tk_str )
			end if
			if has_semi then
				emit_line( "print(" & expr_to_str( expr ) & ", 0);" )
			else
				emit_line( "print(" & expr_to_str( expr ) & ", 1);" )
			end if
		elseif lcase( tk_str ) = "return" then
			dim as node_t ptr expr
			read_token( )
			expr = parse_expression( )
			if match( TK_EOL ) = 0 then
				expected( "end of line", tk_str )
			end if
			emit_line( "return(" & expr_to_str( expr ) & ");" )
		elseif lcase( tk_str ) = "goto" then
			'dim as node_t ptr expr
			dim as string s
			read_token( )
			s = tk_str
			read_token( )
			'expr = parse_expression( )
			if match( TK_EOL ) = 0 then
				expected( "end of line", tk_str )
			end if
			emit_line( "goto " & s & ";" )
		elseif lcase( tk_str ) = "while" then
			dim as node_t ptr expr
			read_token( )
			expr = parse_while( )
			emit_line( "while(" & expr_to_str( expr ) & ") {" )
			indent += 1
		elseif lcase( tk_str ) = "if" then
			dim as node_t ptr expr
			read_token( )
			expr = parse_if( )
			emit_line( "if(" & expr_to_str( expr ) & ") {" )
			indent += 1
			symstack_push( )
		elseif lcase( tk_str ) = "elseif" then
			dim as node_t ptr expr
			read_token( )
			expr = parse_if( )
			indent -= 1
			symstack_pop( )
			emit_line( "} else if(" & expr_to_str( expr ) & ") {" )
			indent += 1
			symstack_push( )
		elseif lcase( tk_str ) = "else" then
			read_token( )
			if match( TK_EOL ) = 0 then
				expected( "end of line", tk_str )
			end if
			indent -= 1
			symstack_pop( )
			emit_line( "} else {" )
			indent += 1
			symstack_push( )
		elseif lcase( tk_str ) = "do" then
			read_token( )
			if match( TK_EOL ) = 0 then
				expected( "end of line", tk_str )
			end if
			emit_line( "do {" )
			indent += 1
		elseif lcase( tk_str ) = "loop" then
			read_token( )
			if match( TK_EOL ) = 0 then
				expected( "end of line", tk_str )
			end if
			indent -= 1
			emit_line( "} while(1);" )
		elseif lcase( tk_str ) = "wend" then
			read_token( )
			if match( TK_EOL ) = 0 then
				expected( "end of line", tk_str )
			end if
			indent -= 1
			emit_line( "}" )
		elseif lcase( tk_str ) = "end" then
			read_token( )
			if match_str( "sub" ) then
				emit_line( "return;" )
				symstack_pop( )
			elseif match_str( "function" ) then
				emit_line( "return func$result;" )
				symstack_pop( )
			elseif match_str( "if" ) then
				symstack_pop( )
			else
				read_token( )
			end if
			if match( TK_EOL ) = 0 then
				expected( "end of line", tk_str )
			end if
			indent -= 1
			emit_line( "}" )
		elseif lcase( tk_str ) = "exit" then
			read_token( )
			read_token( )
			if match( TK_EOL ) = 0 then
				expected( "end of line", tk_str )
			end if
			emit_line( "break;" )
		elseif tk_typ = TK_IDENT then
			dim as datatype_t ptr dt = callocate( sizeof( datatype_t ) )
			dim as string s = parse_lvalue( dt )
			dim as node_t ptr expr
			if tk_str = "=" then
				if match_str( "=" ) = 0 then
					expected( "'='", tk_str )
				end if
				expr = parse_expression( )
				if match( TK_EOL ) = 0 then
					expected( "end of line", tk_str )
				end if
				s += " = (" & dt->s & ")" & expr_to_str( expr )
			elseif tk_str = "+=" then
				if match_str( "+=" ) = 0 then
					expected( "'+='", tk_str )
				end if
				expr = parse_expression( )
				if match( TK_EOL ) = 0 then
					expected( "end of line", tk_str )
				end if
				s += " += " & expr_to_str( expr )
			elseif tk_str = "-=" then
				if match_str( "-=" ) = 0 then
					expected( "'-='", tk_str )
				end if
				expr = parse_expression( )
				if match( TK_EOL ) = 0 then
					expected( "end of line", tk_str )
				end if
				s += " -= " & expr_to_str( expr )
			elseif tk_str = ":" then
				if match_str( ":" ) = 0 then
					expected( "':'", tk_str )
				end if
				if match( TK_EOL ) = 0 then
					expected( "end of line", tk_str )
				end if
				s += ":"
				emit_line( s )
				goto moop
			else
				' function call
				if match( TK_EOL ) = 0 then
					expected( "end of line", tk_str )
				end if
			end if
			emit_line( s & ";" )
		elseif tk_typ = TK_EOL then
			read_token( )
		else
			print "Expected keyword, found '" & tk_str & "'"
			exit_( 1 )
		end if
	wend

end sub

'*******************************************************************************
' Main
'*******************************************************************************

'::::::::
function main _
	( _
		byval argc as integer, _
		byval argv as zstring ptr ptr _
	) as integer

	print "#include <stdlib.h>"
	print "#include <stdio.h>"
	print "#include <string.h>"
	print "#include <ctype.h>"
	print "struct FBSTRING {"
	print !"\tFBSTRING();"
	print !"\tFBSTRING(const char *);"
	print !"\tFBSTRING(const FBSTRING&);"
	print !"\t~FBSTRING();"
	print !"\tint operator=(const char*);"
	print !"\tint operator=(const FBSTRING&);"
	print !"\tint operator==(const FBSTRING&);"
	print !"\tint operator!=(const FBSTRING&);"
	print !"\tint operator+=(const FBSTRING&);"
	'print !"\toperator const FBSTRING&();"
	'print !"\toperator const FBSTRING();"
	'print !"\toperator FBSTRING&();"
	'print !"\toperator FBSTRING();"
	print !"\tvoid *data;"
	print !"\tint len;"
	print !"\tint size;"
	print "};"
	print "typedef int integer;"
	print "typedef char zstring;"
	print "void print(const FBSTRING&, integer nl);"
	print "FBSTRING& concat(const FBSTRING&, const FBSTRING&);"
	print "FBSTRING& lcase(const FBSTRING&);"
	print "FBSTRING& int_to_str(integer);"
	print "FBSTRING& chr(integer);"
	print "integer asc(const FBSTRING&);"
	print "integer val(const FBSTRING&);"
	print "#define exit_ exit"
	print "#define EOF_ EOF"
	print "#define callocate(n) calloc(n, 1 )"
       
	' CONSTRUCTORS

	print "FBSTRING::FBSTRING()"
	print "{"
	print !"\tdata = 0;"
	print !"\tlen = 0;"
	print !"\tsize = 0;"
	print "}"

	print "FBSTRING::FBSTRING(const char *s)"
	print "{"
	print !"\tdata = 0;"
	print !"\tlen = 0;"
	print !"\tsize = 0;"
	print !"\tif(s == 0) return;"
	print !"\tint s_len = strlen(s);"
	print !"\tdata = malloc(s_len);"
	print !"\tlen = s_len;"
	print !"\tsize = s_len;"
	print !"\tmemcpy(data, s, s_len);"
	print "}"

	print "FBSTRING::FBSTRING(const FBSTRING& s)"
	print "{"
	print !"\tdata = 0;"
	print !"\tlen = 0;"
	print !"\tsize = 0;"
	print !"\tif(s.data == 0) return;"
	print !"\tdata = malloc(s.len);"
	print !"\tlen = s.len;"
	print !"\tsize = s.len;"
	print !"\tmemcpy(data, s.data, s.len);"
	print "}"

	' DTOR

	print "FBSTRING::~FBSTRING()"
	print "{"
	'print !"\tif(data) free(data);"
	print !"\tdata = 0;"
	print !"\tlen = 0;"
	print !"\tsize = 0;"
	print "}"

	' LET

	print "int FBSTRING::operator=(const char *s)"
	print "{"
	print !"\tif(data) free(data);"
	print !"\tdata = 0;"
	print !"\tlen = 0;"
	print !"\tsize = 0;"
	print !"\tif(s == 0) return 0;"
	print !"\tint s_len = strlen(s);"
	print !"\tdata = malloc(s_len);"
	print !"\tlen = s_len;"
	print !"\tsize = s_len;"
	print !"\tmemcpy(data, s, s_len);"
	print !"\treturn 0;"
	print "}"

	print "int FBSTRING::operator=(const FBSTRING& s)"
	print "{"
	'print !"\tif(data) free(data);"
	print !"\tdata = 0;"
	print !"\tlen = 0;"
	print !"\tsize = 0;"
	print !"\tif(s.data == 0) return 0;"
	print !"\tdata = malloc(s.len);"
	print !"\tlen = s.len;"
	print !"\tsize = s.len;"
	print !"\tmemcpy(data, s.data, s.len);"
	print !"\treturn 0;"
	print "}"

	' REL OP

	print !"int FBSTRING::operator==(const FBSTRING& s)"
	print "{"
	print !"\tif(len != s.len) return 0;"
	print !"\tif((data == 0) && (s.data == 0)) return 1;"
	print !"\tif((data == 0) || (s.data == 0)) return 0;"
	print !"\tif(memcmp(data, s.data, len) != 0) return 0;"
	print !"\treturn 1;"
	print "}"

	print !"int FBSTRING::operator!=(const FBSTRING& s)"
	print "{"
	print !"\tif(len != s.len) return 1;"
	print !"\tif((data == 0) && (s.data == 0)) return 0;"
	print !"\tif((data == 0) || (s.data == 0)) return 1;"
	print !"\tif(memcmp(data, s.data, len) != 0) return 1;"
	print !"\treturn 0;"
	print "}"

	' SELF OP

	print !"int FBSTRING::operator+=(const FBSTRING& s)"
	print "{"
	print !"\tFBSTRING result;"
	print !"\tresult.data = data;"
	print !"\tresult.len = len;"
	print !"\tresult.size = size;"
	print !"\tresult = concat(result, s);"
	print !"\tfree(data);"
	print !"\tdata = result.data;"
	print !"\tlen = result.len;"
	print !"\tsize = result.size;"
	print !"\treturn 0;"
	print "}"




	print "void print(const FBSTRING& s, integer nl)"
	print "{"
	print !"\tint pos;"
	print !"\tfor(pos = 0; pos < s.len; pos++) {"
	print !"\t\tfputc(((char *)s.data)[pos], stdout);"
	print !"\t}"
	print !"\tif(nl) fputc(10, stdout);"
	print "}"

	print "FBSTRING& concat(const FBSTRING& s1, const FBSTRING& s2)"
	print "{"
	print !"\tFBSTRING *result = (FBSTRING *)calloc(sizeof(FBSTRING), 1);"
	print !"\tresult->data = malloc(s1.len + s2.len);"
	print !"\tresult->len = s1.len + s2.len;"
	print !"\tresult->size = s1.len + s2.len;"
	print !"\tmemcpy(result->data, s1.data, s1.len);"
	print !"\tmemcpy(&(((char *)result->data)[s1.len]), s2.data, s2.len);"
	print !"\treturn *result;"
	print "}"

	print "FBSTRING& lcase(const FBSTRING& s)"
	print "{"
	print !"\tFBSTRING *result = (FBSTRING *)calloc(sizeof(FBSTRING), 1);"
	print !"\t*result = s;"
	print !"\tint pos;"
	print !"\tfor(pos = 0; pos < s.len; pos++) {"
	print !"\t\t((char *)result->data)[pos] = tolower(((char *)s.data)[pos]);"
	print !"\t}"
	print !"\treturn *result;"
	print "}"

	print "FBSTRING& str_temp(const FBSTRING& s)"
	print "{"
	print !"\tFBSTRING *result = (FBSTRING *)calloc(sizeof(FBSTRING), 1);"
	print !"\t*result = s;"
	print !"\treturn *result;"
	print "}"

	print "FBSTRING& int_to_str(integer i)"
	print "{"
	print !"\tFBSTRING *result = (FBSTRING *)calloc(sizeof(FBSTRING), 1);"
	print !"\tchar *tmp = (char *)calloc(32, 1);"
	print !"\tsprintf(tmp, " & chr( 34 ) & "%i" & chr( 34 ) & ", i);"
	print !"\tint tmp_len = strlen(tmp);"
	print !"\tresult->data = tmp;"
	print !"\tresult->len = tmp_len;"
	print !"\tresult->size = 32;"
	print !"\treturn *result;"
	print "}"

	print "FBSTRING& chr(integer i)"
	print "{"
	print !"\tFBSTRING *result = (FBSTRING *)calloc(sizeof(FBSTRING), 1);"
	print !"\tresult->data = malloc(1);"
	print !"\tresult->len = 1;"
	print !"\tresult->size = 1;"
	print !"\t((char *)result->data)[0] = i;"
	print !"\treturn *result;"
	print "}"

	print "integer val(const FBSTRING& s)"
	print "{"
	print !"\tint n;"
	print !"\tchar *tmp = (char *)calloc(s.len + 1, 1);"
	print !"\tmemcpy(tmp, s.data, s.len);"
	print !"\tn = atoi(tmp);"
	print !"\tfree(tmp);"
	print !"\treturn n;"
	print "}"

	print "integer asc(const FBSTRING& s)"
	print "{"
	print !"\tif(s.data == 0) return -1;"
	print !"\treturn ((char *)s.data)[0];"
	print "}"

	sym_add_proc( "lcase" )
	sym_add_proc( "exit_" )
	sym_add_proc( "EOF_" )
	sym_add_proc( "fgetc" )
	sym_add_proc( "asc" )
	sym_add_proc( "isdigit" )
	sym_add_proc( "isalnum" )
	sym_add_proc( "isalpha" )
	sym_add_proc( "callocate" )
	sym_add_proc( "sizeof" )
	sym_add_proc( "chr" )
	sym_add_proc( "val" )
	dim as datatype_t ptr dt = callocate( sizeof( datatype_t ) )
	dt->s = "integer"
	sym_add_dim( "__FUNCTION__", dt, 0, 0, -1 )
	sym_add_dim( "stdin", dt, 0, 0, -1 )

	read_char( )
	read_token( )
       
	parse_file( )
       
	if tk_typ <> TK_EOF then
		print "FAILED!"
	end if

	function = 0

end function

'::::::::
rem main( __FB_ARGC__, __FB_ARGV__ )
