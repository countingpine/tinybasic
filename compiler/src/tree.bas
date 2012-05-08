#include once "compiler/src/inc/error.bi"

'::::::::
function tree_node_litint _
	( _
		byref s as string _
	) as node_t ptr

	dim as node_t ptr node = callocate( sizeof( node_t ) )

	node->typ    = NODE_LITINT
	node->_i     = s
	node->dt._dt = DT_INTEGER

	function = node

end function

'::::::::
function tree_node_litstr _
	( _
		byref s as string _
	) as node_t ptr

	dim as node_t ptr node = callocate( sizeof( node_t ) )

	node->typ    = NODE_LITSTR
	node->_s     = s
	node->dt._dt = DT_LITSTR

	function = node

end function

'::::::::
function tree_node_convert _
	( _
		byval l as node_t ptr, _
		byval dt as datatype_t ptr _
	) as node_t ptr

	if (l->dt._dt = dt->_dt) and (l->dt._ptr_cnt = dt->_ptr_cnt) and (l->dt._sym = dt->_sym) then
		function = l
	else
		dim as node_t ptr node = callocate( sizeof( node_t ) )
	
		node->typ = NODE_CONVERT
		node->l   = l
		node->dt  = *dt
	
		function = node
	end if

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
	'print expr_cnt
	while i < expr_cnt
		node->expr(i) = expr[i]
		i += 1
	wend

	'print "!!"
	'print sym
	'print sym->dt

	dim as datatype_t ptr dt = sym->dt

	if dt then
		node->dt = *dt
	else
		' if a sub call
	end if

	function = node

	if sym->ident = "ASC" then
		if node->expr(0)->typ = NODE_LITSTR then
			dim as string s
			s = "" & asc( node->expr(0)->_s )
			node->expr(0)->_s = ""
			free(node->expr(0))
			free(node)
			function = tree_node_litint( s )
		end if
	end if

end function

'::::::::
function tree_node_bop _
	( _
		byref bop as string, _
		byval l as node_t ptr, _
		byval r as node_t ptr _
	) as node_t ptr

	dim as node_t ptr node = callocate( sizeof( node_t ) )
	dim as datatype_t ptr dt = callocate( sizeof( datatype_t ) )
	dim as integer is_string_compare

	node->typ  = NODE_BOP
	node->_bop = bop
	node->l    = l
	node->r    = r

	dt->_dt = DT_STRING

	' Promote to string
	if (bop = "=") or (bop = "<>") then
		if (node->l->dt._dt = DT_STRING) or (node->r->dt._dt = DT_STRING) then
			if (node->l->dt._dt = DT_STRING) then
				if (node->l->dt._ptr_cnt = 0) then
					node->l = tree_node_convert( node->l, dt )
					node->r = tree_node_convert( node->r, dt )
				else
					die( "Unhandled ptr cnt DT_STRING l" )
				end if
			else
				if (node->r->dt._ptr_cnt = 0) then
					node->l = tree_node_convert( node->l, dt )
					node->r = tree_node_convert( node->r, dt )
				else
					die( "Unhandled ptr cnt DT_STRING r" )
				end if
			end if
			is_string_compare = -1
		elseif (node->l->dt._dt = DT_LITSTR) or (node->r->dt._dt = DT_LITSTR) then
			if (node->l->dt._dt = DT_LITSTR) then
				if (node->l->dt._ptr_cnt = 0) then
					node->l = tree_node_convert( node->l, dt )
					node->r = tree_node_convert( node->r, dt )
				else
					die( "Unhandled ptr cnt DT_LITSTR l" )
				end if
			else
				if (node->r->dt._ptr_cnt = 0) then
					node->l = tree_node_convert( node->l, dt )
					node->r = tree_node_convert( node->r, dt )
				else
					die( "Unhandled ptr cnt DT_LITSTR r" )
				end if
			end if
			is_string_compare = -1
		elseif (node->l->dt._dt = DT_ZSTRING) or (node->r->dt._dt = DT_ZSTRING) then
			if (node->l->dt._dt = DT_ZSTRING) then
				if (node->l->dt._ptr_cnt = 0) then
					node->l = tree_node_convert( node->l, dt )
					node->r = tree_node_convert( node->r, dt )
				else
					' a comparison to a pointer value
					goto end_string_fixup
					'die( "Unhandled ptr cnt DT_ZSTRING l" & node->l->dt._ptr_cnt )
				end if
			else
				if (node->r->dt._ptr_cnt = 0) then
					node->l = tree_node_convert( node->l, dt )
					node->r = tree_node_convert( node->r, dt )
				else
					die( "Unhandled ptr cnt DT_ZSTRING r" )
				end if
			end if
			is_string_compare = -1
		end if
	end if

	end_string_fixup:

	'if bop = "<>" then
	'	if node->l->dt._ptr_cnt > 0 then
	'		die( "Unknown conversion ptr 1" )
	'	elseif node->r->dt._ptr_cnt > 0 then
	'		die( "Unknown conversion ptr 2" )
	'	elseif node->l->dt._dt <> node->r->dt._dt then
	'		if node->l->dt._dt = DT_STRING then
	'			if node->r->dt._dt = DT_LITSTR then
	'				node->r = tree_node_convert( node->r, @node->l->dt )
	'			else
	'				die( "Unknown conversion l" )
	'			end if
	'		elseif node->r->dt._dt = DT_STRING then
	'			if node->l->dt._dt = DT_LITSTR then
	'				node->l = tree_node_convert( node->l, @node->r->dt )
	'			else
	'				die( "Unknown conversion r " & node->l->dt._dt )
	'			end if
	'		end if
	'	end if
	'end if

	

	if (bop = "=") or (bop = "<>") or (bop = "<") or (bop = ">") or (bop = "<=") or (bop = ">=") then
		node->dt._dt = DT_INTEGER
		node->dt._ptr_cnt = 0
		node->dt._sym = 0
		if is_string_compare then
			if bop = "=" then
				dim as node_t ptr l = node->l
				dim as node_t ptr r = node->r
				dim as sym_t ptr eq_sym = sym_find( "TBSTRING_EQ", 0 )
				node->_bop = ""
				free(node)
				node = tree_node_proccall( eq_sym, 0, 0 )
				node->expr(0) = l
				node->expr(1) = r
				node->expr_cnt = 2
			elseif bop = "<>" then
				dim as node_t ptr l = node->l
				dim as node_t ptr r = node->r
				dim as sym_t ptr eq_sym = sym_find( "TBSTRING_NE", 0 )
				node->_bop = ""
				free(node)
				node = tree_node_proccall( eq_sym, 0, 0 )
				node->expr(0) = l
				node->expr(1) = r
				node->expr_cnt = 2
			else
				die( "Bad string op" )
			end if
		end if
	elseif (bop = ".") or (bop = "->") then
		node->dt = node->r->dt
	else
		node->dt = node->l->dt
	end if

	free(dt)

	function = node

end function

'::::::::
function tree_node_uop _
	( _
		byref uop as string, _
		byval l as node_t ptr _
	) as node_t ptr

	dim as node_t ptr node = callocate( sizeof( node_t ) )

	node->typ  = NODE_UOP
	node->_uop = uop
	node->l    = l

	node->dt = l->dt

	if uop = "*" then
		if node->dt._ptr_cnt <= 0 then
			die( "Cannot dereference that!" )
		else
			node->dt._ptr_cnt -= 1
		end if
	elseif uop = "&" then
		node->dt._ptr_cnt += 1
	end if

	function = node

end function

'::::::::
function tree_node_array_access _
	( _
		byval sym as sym_t ptr, _
		byval expr as node_t ptr ptr, _
		byval expr_cnt as integer _
	) as node_t ptr

	dim as node_t ptr node = callocate( sizeof( node_t ) )

	node->typ = NODE_ARRAY_ACCESS
	node->sym = sym
	node->expr_cnt = expr_cnt

	dim as integer i = 0
	while i < expr_cnt
		node->expr(i) = expr[i]
		i += 1
	wend

	dim as datatype_t ptr dt = sym->dt

	node->dt = *dt

	function = node

end function

'::::::::
function tree_node_ptr_array_access _
	( _
		byval sym as sym_t ptr, _
		byval expr as node_t ptr ptr, _
		byval expr_cnt as integer _
	) as node_t ptr

	dim as node_t ptr node = callocate( sizeof( node_t ) )

	node->typ = NODE_PTR_ARRAY_ACCESS
	node->sym = sym
	node->expr_cnt = expr_cnt

	dim as integer i = 0
	while i < expr_cnt
		node->expr(i) = expr[i]
		i += 1
	wend

	dim as datatype_t ptr dt = sym->dt

	node->dt = *dt

	if node->dt._ptr_cnt <= 0 then
		die( "Cannot access that!" )
	else
		node->dt._ptr_cnt -= 1
	end if

	function = node

end function

'::::::::
function tree_node_var _
	( _
		byval sym as sym_t ptr _
	) as node_t ptr

	dim as node_t ptr node = callocate( sizeof( node_t ) )

	node->typ = NODE_VAR
	node->sym = sym

	dim as datatype_t ptr dt = sym->dt

	node->dt = *dt

	function = node

end function

'::::::::
function tree_node_enum_val _
	( _
		byval sym as sym_t ptr _
	) as node_t ptr

	dim as node_t ptr node = callocate( sizeof( node_t ) )

	node->typ = NODE_ENUM_VAL
	node->sym = sym

	'dim as datatype_t ptr dt = sym->dt

	node->dt._dt = DT_INTEGER

	function = node

end function

'::::::::
function tree_node_type _
	( _
		byval sym as sym_t ptr _
	) as node_t ptr

	dim as node_t ptr node = callocate( sizeof( node_t ) )

	node->typ = NODE_TYPE
	node->sym = sym

	dim as datatype_t ptr dt = sym->dt

	if dt then
		node->dt = *dt
	end if

	function = node

end function

'::::::::
function tree_node_type_fwd_decl _
	( _
		byval sym as sym_t ptr _
	) as node_t ptr

	dim as node_t ptr node = callocate( sizeof( node_t ) )

	node->typ = NODE_TYPE_FWD_DECL
	node->sym = sym

	function = node

end function

'::::::::
function tree_node_label _
	( _
		byval sym as sym_t ptr _
	) as node_t ptr

	dim as node_t ptr node = callocate( sizeof( node_t ) )

	node->typ = NODE_LABEL
	node->sym = sym

	'dim as datatype_t ptr dt = sym->dt

	'node->dt = *dt

	function = node

end function

'::::::::
function tree_node_proc _
	( _
		byval sym as sym_t ptr, _
		byval stmt_list as node_t ptr, _
		byval symtab as symtab_t ptr _
	) as node_t ptr

	dim as node_t ptr node = callocate( sizeof( node_t ) )

	node->typ = NODE_PROC
	node->sym = sym
	node->stmt_list = stmt_list
	node->symtab = symtab

	function = node

end function

'::::::::
function tree_node_proc_decl _
	( _
		byval sym as sym_t ptr, _
		byval symtab as symtab_t ptr _
	) as node_t ptr

	dim as node_t ptr node = callocate( sizeof( node_t ) )

	node->typ = NODE_PROC_DECL
	node->sym = sym
	node->symtab = symtab

	function = node

end function

'::::::::
'function tree_node_field _
'	( _
'		byval sym as sym_t ptr _
'	) as node_t ptr
'
'	dim as node_t ptr node = callocate( sizeof( node_t ) )
'
'	node->typ = NODE_FIELD
'	node->sym = sym
'	node->expr_cnt = expr_cnt
'
'	dim as integer i = 0
'	while i < expr_cnt
'		node->expr(i) = expr[i]
'		i += 1
'	wend
'
'	function = node
'
'end function


'::::::::
'function tree_node_lvalue _
'	( _
'		byref s as string, _
'		byval dt as datatype_t ptr _
'	) as node_t ptr
'
'	if dt->expr_cnt > 0 then
'		if dt->is_proc then
'			dim as node_t ptr ptr expr = @dt->expr(0)
'			dim as node_t ptr node = tree_node_proccall( dt->sym, expr, dt->expr_cnt )
'			return node
'		end if
'	end if
'
'	dim as node_t ptr node = callocate( sizeof( node_t ) )
'
'	node->typ = NODE_LVALUE
'	node->s   = s
'	node->dt.s = dt->s
'	node->dt.ptr_cnt = dt->ptr_cnt
'
'	function = node
'
'end function

'::::::::
function tree_node_stmt_list _
	( _
		byval _data as node_t ptr, _
		byval _next as node_t ptr _
	) as node_t ptr

	dim as node_t ptr node = callocate( sizeof( node_t ) )

	node->typ  = NODE_STMT_LIST
	node->_data = _data
	node->_next = _next

	function = node

end function

'::::::::
function tree_node_type_decl _
	( _
		byval sym as sym_t ptr _
	) as node_t ptr

	dim as node_t ptr node = callocate( sizeof( node_t ) )

	node->typ = NODE_TYPE_DECL
	node->sym = sym

	function = node

end function

'::::::::
function tree_node_enum _
	( _
		byval sym as sym_t ptr _
	) as node_t ptr

	dim as node_t ptr node = callocate( sizeof( node_t ) )

	node->typ = NODE_ENUM
	node->sym = sym

	function = node

end function

'::::::::
function tree_node_assign _
	( _
		byval l as node_t ptr, _
		byval r as node_t ptr _
	) as node_t ptr

	dim as node_t ptr node = callocate( sizeof( node_t ) )

	node->typ = NODE_ASSIGN
	node->l = l
	node->r = r

	if (l->dt._dt = r->dt._dt) and (l->dt._ptr_cnt = r->dt._ptr_cnt) and (l->dt._sym = r->dt._sym) then
		
	else
		node->r = tree_node_convert( node->r, @l->dt )
	end if

	if node->l->dt._dt = DT_STRING then
		dim as node_t ptr l = node->l
		dim as node_t ptr r = node->r
		dim as sym_t ptr assign_sym = sym_find( "TBSTRING_assign", 0 )
		free(node)
		node = tree_node_proccall( assign_sym, 0, 0 )
		node->expr(0) = l
		node->expr(1) = r
		node->expr_cnt = 2
	end if

	function = node

end function

'::::::::
function tree_node_assign_result _
	( _
		byval l as node_t ptr _
	) as node_t ptr

	dim as node_t ptr node = callocate( sizeof( node_t ) )

	node->typ = NODE_ASSIGN_RESULT
	node->l = l

	function = node

end function

'::::::::
function tree_node_selfadd _
	( _
		byval l as node_t ptr, _
		byval r as node_t ptr _
	) as node_t ptr

	dim as node_t ptr node = callocate( sizeof( node_t ) )

	node->typ = NODE_SELFADD
	node->l = l
	node->r = r

	if (l->dt._dt = r->dt._dt) and (l->dt._ptr_cnt = r->dt._ptr_cnt) and (l->dt._sym = r->dt._sym) then
		
	else
		node->r = tree_node_convert( node->r, @l->dt )
	end if

	if node->l->dt._dt = DT_STRING then
		dim as node_t ptr l = node->l
		dim as node_t ptr r = node->r
		dim as sym_t ptr assign_sym = sym_find( "TBSTRING_append", 0 )
		free(node)
		node = tree_node_proccall( assign_sym, 0, 0 )
		node->expr(0) = l
		node->expr(1) = r
		node->expr_cnt = 2
	end if

	function = node

end function

'::::::::
function tree_node_selfsub _
	( _
		byval l as node_t ptr, _
		byval r as node_t ptr _
	) as node_t ptr

	dim as node_t ptr node = callocate( sizeof( node_t ) )

	node->typ = NODE_SELFSUB
	node->l = l
	node->r = r

	function = node

end function

'::::::::
function tree_node_dummy _
	( _
		byref dummy as string _
	) as node_t ptr

	dim as node_t ptr node = callocate( sizeof( node_t ) )

	node->typ = NODE_DUMMY
	node->dummy = dummy

	function = node

end function

'::::::::
function tree_node_if _
	( _
		byval if_expr      as node_t ptr, _
		byval if_stmt_list as node_t ptr, _
		byval elseif_list  as node_t ptr, _
		byval else_node    as node_t ptr, _
		byval symtab       as symtab_t ptr _
	) as node_t ptr

	dim as node_t ptr node = callocate( sizeof( node_t ) )

	node->typ          = NODE_IF
	node->if_expr      = if_expr
	node->if_stmt_list = if_stmt_list
	node->elseif_list  = elseif_list
	node->else_node    = else_node
	node->symtab       = symtab

	function = node

end function

'::::::::
function tree_node_for _
	( _
		byval index_expr    as node_t ptr, _
		byval from_expr     as node_t ptr, _
		byval to_expr       as node_t ptr, _
		byval for_stmt_list as node_t ptr, _
		byval symtab        as symtab_t ptr _
	) as node_t ptr

	dim as node_t ptr node = callocate( sizeof( node_t ) )

	node->typ           = NODE_FOR
	node->index_expr    = index_expr
	node->from_expr     = from_expr
	node->to_expr       = to_expr
	node->for_stmt_list = for_stmt_list
	node->symtab        = symtab

	function = node

end function

'::::::::
function tree_node_elseif _
	( _
		byval expr      as node_t ptr, _
		byval stmt_list as node_t ptr, _
		byval symtab    as symtab_t ptr _
	) as node_t ptr

	dim as node_t ptr node = callocate( sizeof( node_t ) )

	node->typ        = NODE_ELSEIF
	node->_expr      = expr
	node->_stmt_list = stmt_list
	node->symtab     = symtab

	function = node

end function

'::::::::
function tree_node_else _
	( _
		byval stmt_list as node_t ptr, _
		byval symtab    as symtab_t ptr _
	) as node_t ptr

	dim as node_t ptr node = callocate( sizeof( node_t ) )

	node->typ        = NODE_ELSE
	node->_stmt_list = stmt_list
	node->symtab     = symtab

	function = node

end function

'::::::::
function tree_node_while _
	( _
		byval expr      as node_t ptr, _
		byval stmt_list as node_t ptr, _
		byval symtab    as symtab_t ptr _
	) as node_t ptr

	dim as node_t ptr node = callocate( sizeof( node_t ) )

	node->typ        = NODE_WHILE
	node->_expr      = expr
	node->_stmt_list = stmt_list
	node->symtab     = symtab

	function = node

end function

'::::::::
function tree_node_return _
	( _
		byval expr as node_t ptr _
	) as node_t ptr

	dim as node_t ptr node = callocate( sizeof( node_t ) )

	node->typ        = NODE_RETURN
	node->_expr      = expr

	function = node

end function

'::::::::
function tree_node_print _
	( _
		byval expr as node_t ptr, _
		byval do_newline as integer _
	) as node_t ptr

	dim as node_t ptr node = callocate( sizeof( node_t ) )

	node->typ         = NODE_PRINT
	node->_expr       = expr
	node->_do_newline = do_newline

	function = node

end function

'::::::::
function tree_node_goto _
	( _
		byref label as string _
	) as node_t ptr

	dim as node_t ptr node = callocate( sizeof( node_t ) )

	node->typ    = NODE_GOTO
	node->_label = label

	function = node

end function

'::::::::
function tree_node_elseif_list _
	( _
		byval _data as node_t ptr, _
		byval _next as node_t ptr _
	) as node_t ptr

	dim as node_t ptr node = callocate( sizeof( node_t ) )

	node->typ  = NODE_ELSEIF_LIST
	node->_data = _data
	node->_next = _next

	function = node

end function

'::::::::
function tree_node_break _
	( _
	) as node_t ptr

	dim as node_t ptr node = callocate( sizeof( node_t ) )

	node->typ = NODE_BREAK

	function = node

end function
