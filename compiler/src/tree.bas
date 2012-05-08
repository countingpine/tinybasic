#include once "inc/error.bi"
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
