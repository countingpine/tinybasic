#include once "inc/node.bi"
#include once "inc/error.bi"
#include once "inc/param_info.bi"
#include once "inc/consts.bi"
#include once "inc/tree.bi"

'::::::::
declare function expr_to_str _
	( _
		byval node as node_t ptr _
	) as string

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

