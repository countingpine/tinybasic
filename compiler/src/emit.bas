#include once "compiler/src/inc/node.bi"
#include once "compiler/src/inc/error.bi"
#include once "compiler/src/inc/param_info.bi"
#include once "compiler/src/inc/consts.bi"
#include once "compiler/src/inc/tree.bi"
#include once "compiler/src/inc/help.bi"

'::::::::
sub emit_line _
	( _
		byval out_hfile as FILE ptr, _
		byref txt as string _
	)

	dim as integer i
	dim as string s

	i = 0
	while i < indent
		s += !"\t"
		i += 1
	wend

	s += txt & !"\n"

	fputs( s, out_hfile )

end sub

'::::::::
function datatype_to_str _
	( _
		byval dt as datatype_t ptr _
	) as string

	dim as string s
	dim as integer i
	dim as integer typ

	if dt->_sym then
		typ = dt->_sym->typ
	end if

	if typ = SYM_DATATYPE_FWD then
		s += dt->_sym->_real
	elseif dt->_dt = DT_INTEGER then
		s += "integer"
	elseif dt->_dt = DT_UINTEGER then
		s += "uinteger"
	elseif dt->_dt = DT_STRING then
		s += "TBSTRING"
	elseif dt->_dt = DT_ANY then
		s += "void"
	elseif dt->_dt = DT_ZSTRING then
		s += "char"
	elseif dt->_dt = DT_TYPE then
		if dt->_sym then
			s += dt->_sym->ident
		else
			s += "whut"
		end if
	else
		die( "datatype error" )
	end if

	while i < dt->_ptr_cnt
		if i = 0 then
			s += " "
		end if
		s += "*"
		i += 1
	wend	

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
		bop = node->_bop
		l = expr_to_str( node->l )
		r = expr_to_str( node->r )
		if bop = "=" then
			bop = " == "
		elseif bop = "<>" then
			bop = " != "

		elseif bop = "<" then
			bop = " < "
		elseif bop = ">" then
			bop = " > "
		elseif bop = "<=" then
			bop = " <= "
		elseif bop = ">=" then
			bop = " >= "
		elseif ucase( bop ) = "AND" then
			bop = " & "
		elseif ucase( bop ) = "OR" then
			bop = " | "
		elseif bop = "&" then
			bop = "concat"
		end if
		if bop = "concat" then
			function = "concat(" & l & ", " & r & ")"
		else
			function = "(" & l & bop & r & ")"
		end if
	elseif node->typ = NODE_UOP then
		dim as string uop
		dim as string l
		uop = node->_uop
		l = expr_to_str( node->l )
		if uop = "@" then
			uop = "&"
		end if
		function = "(" & uop & l & ")"
	elseif node->typ = NODE_CONVERT then
		dim as string l
		l = expr_to_str( node->l )
		if node->dt._dt = DT_STRING then
			if node->l->dt._dt = DT_LITSTR then
				if node->l->dt._ptr_cnt = 0 then
					function = "zstr_temp(" & l & ")"
				else
					die( "nothing known!!1" )
				end if
			elseif node->l->dt._dt = DT_ZSTRING then
				if node->l->dt._ptr_cnt = 0 then
					function = "zstr_temp(&" & l & ")"
				else
					function = "int_to_str(" & l & ")"
				end if
			elseif node->l->dt._ptr_cnt > 0 then
				function = "int_to_str(" & l & ")"
			elseif node->l->dt._dt = DT_INTEGER then
				function = "int_to_str(" & l & ")"
			else
				die( "No convert known!" & node->l->dt._dt )
			end if
		else
			function = "((" & datatype_to_str( @node->dt ) & ")" & l & ")"
		end if
	elseif node->typ = NODE_PROCCALL then
		dim as string s
		s = node->sym->_alias & "("
		dim as integer i = 0
		while i < node->expr_cnt
			dim as param_info_t ptr pi = node->sym->pi
			if pi[i].dt->_dt = DT_STRING then
				if node->expr(i)->dt._dt = DT_LITSTR then
					s += "zstr_temp(" & expr_to_str( node->expr(i) ) & ")"
				else
					s += expr_to_str( node->expr(i) )
				end if
			elseif pi[i].dt->_dt = DT_ZSTRING then
				if pi[i].dt->_ptr_cnt = 1 then
					if node->expr(i)->dt._dt = DT_STRING then
						s += "TBSTRING_to_zstrp(" & expr_to_str( node->expr(i) ) & ")"
					else
						s += expr_to_str( node->expr(i) )
					end if
				else
					s += "(" & datatype_to_str( pi[i].dt ) & ")" & expr_to_str( node->expr(i) )
				end if
			elseif pi[i].dt->_dt = DT_ANY then
				s += expr_to_str( node->expr(i) )
			else
				dim as integer are_same
				if pi[i].dt->_dt = node->expr(i)->dt._dt then
					if pi[i].dt->_ptr_cnt = node->expr(i)->dt._ptr_cnt then
						if pi[i].dt->_sym = node->expr(i)->dt._sym then
							are_same = -1
						end if
					end if
				end if
				if are_same then
					s += expr_to_str( node->expr(i) )
				else
					s += "(" & datatype_to_str( pi[i].dt ) & ")" & expr_to_str( node->expr(i) )
				end if
			end if
			i += 1
			if i < node->expr_cnt then
				s += ", "
			end if
		wend
		s += ")"
		function = s
	elseif node->typ = NODE_LITINT then
		function = node->_i
	elseif node->typ = NODE_LITSTR then
		function = "((char *)" & chr( CHAR_DBLQUOTE ) & node->_s & chr( CHAR_DBLQUOTE ) & ")"
	elseif (node->typ = NODE_VAR) then
		function = node->sym->_alias
	elseif node->typ = NODE_ENUM_VAL then
		function = node->sym->ident
	elseif node->typ = NODE_TYPE then
		function = node->sym->_alias
	elseif node->typ = NODE_LABEL then
		function = node->sym->ident & ":"
	elseif (node->typ = NODE_ARRAY_ACCESS) or (node->typ = NODE_PTR_ARRAY_ACCESS) then
		dim as string s
		s = node->sym->ident & "["
		dim as integer i = 0
		while i < node->expr_cnt
			s += expr_to_str( node->expr(i) )
			i += 1
			if i < node->expr_cnt then
				s += ", "
			end if
		wend
		s += "]"
		function = s
	else
		die( "UNKNOWN NODE!" & node->typ )
	end if

end function

'::::::::
function proc_header_to_str _
	( _
		byval proc as node_t ptr _
	) as string

	dim as string            s
	dim as integer           i

	s += !"/*::::::::*/\n"

	if proc->sym->dt then
		s += datatype_to_str( proc->sym->dt ) & " "
	else
		s += "void "
	end if

	s += proc->sym->ident & "("

	'print "/* CALL " & proc->sym->ident & " " & proc->sym->p_count & " */"

	i = 0
	while i < proc->sym->p_count
		dim as param_info_t ptr pi = proc->sym->pi
		if pi[i].is_byref then
			's += "const "
		end if
		s += datatype_to_str( pi[i].dt )
		if pi[i].is_byref then
			s += "&"
		end if
		s += " " & pi[i].ident
		i += 1
		if i <> proc->sym->p_count then
			s += ", "
		end if
	wend
	
	s += ")"

	function = s

end function

sub emit_header _
	( _
		byval out_hfile as FILE ptr _
	)

	emit_line( out_hfile, "#include <stdlib.h>" )
	emit_line( out_hfile, "#include <stdio.h>" )
	emit_line( out_hfile, "#include <string.h>" )
	emit_line( out_hfile, "#include <ctype.h>" )
	'emit_line( out_hfile, "#include <error.h>" )
	emit_line( out_hfile, "#include <unistd.h>" )

	emit_line( out_hfile, "#include " & chr( CHAR_DBLQUOTE ) & "rtlib/src/inc/rtlib.h" & chr( CHAR_DBLQUOTE ) )

	emit_line( out_hfile, "typedef signed int integer;" )
	emit_line( out_hfile, "typedef unsigned int uinteger;" )
	emit_line( out_hfile, "typedef char zstring;" )

	emit_line( out_hfile, "#define exit_ exit" )
	emit_line( out_hfile, "#define error_ error" )
	emit_line( out_hfile, "#define system_ system" )
	emit_line( out_hfile, "#define EOF_ EOF" )
	emit_line( out_hfile, "#define callocate(n) calloc(n, 1 )" )

end sub


declare sub emit_tree _
	( _
		byval out_hfile as FILE ptr, _
		byval node as node_t ptr _
	)

sub emit_if _
	( _
		byval out_hfile as FILE ptr, _
		byval node as node_t ptr _
	)

	emit_line( out_hfile, "if(" & expr_to_str( node->if_expr ) & ") {" )
	indent += 1
	emit_tree( out_hfile, node->if_stmt_list )
	indent -= 1
	emit_tree( out_hfile, node->elseif_list )
	if node->else_node then
		emit_line( out_hfile, "} else {" )
		indent += 1
		emit_tree( out_hfile, node->else_node )
		indent -= 1
	end if
	emit_line( out_hfile, "}" )

end sub

sub emit_while _
	( _
		byval out_hfile as FILE ptr, _
		byval node as node_t ptr _
	)

	emit_line( out_hfile, "while(" & expr_to_str( node->_expr ) & ") {" )
	indent += 1
	emit_tree( out_hfile, node->_stmt_list )
	indent -= 1
	emit_line( out_hfile, "}" )

end sub

sub emit_enum _
	( _
		byval out_hfile as FILE ptr, _
		byval node as node_t ptr _
	)

	dim as string s
	dim as integer i

	emit_line( out_hfile, "/*::::::::*/" )
	emit_line( out_hfile, "enum " & node->sym->ident & " {" )

	indent += 1

	i = 0
	while i < node->sym->member_count
		s = node->sym->members[i].ident & " = " & node->sym->members[i].enum_val
		if i = node->sym->member_count - 1 then

		else
			s += ","
		end if
		emit_line( out_hfile, s )
		i += 1
	wend

	indent -= 1

	emit_line( out_hfile, "};" )

end sub

'::::::::
sub emit_dim_item _
	( _
		byval out_hfile as FILE ptr, _
		byval is_shared as integer, _
		byref item      as dim_item_t _
	)

	dim as string  s
	dim as integer do_memset
	dim as integer array_size
	dim as string  clrtext

	dim as datatype_t ptr dt = item.dt

	s = datatype_to_str( dt ) & " " & item.sym->ident

	if item.is_array then
		array_size = val( item.bounds.upper_expr->_i ) + 1
		s += "[" & array_size & "]"
	end if

	if item.init_expr = 0 then
		if (dt->_dt = DT_INTEGER) or (dt->_dt = DT_UINTEGER) then
			if item.is_array = 0 then
				item.init_expr = tree_node_litint( "0" )
			else
				clrtext = "memset((void *)"
				clrtext += item.sym->ident
				clrtext += ", 0, "
				clrtext += "" & (get_dt_size( dt ) * array_size)
				clrtext += ");"
				do_memset = -1
			end if
		elseif dt->_ptr_cnt > 0 then
			if item.is_array = 0 then
				item.init_expr = tree_node_litint( "0" )
			else
				clrtext = "memset((void *)"
				clrtext += item.sym->ident
				clrtext += ", 0, "
				clrtext += "" & (get_dt_size( dt ) * array_size)
				clrtext += ");"
				do_memset = -1
			end if
		elseif dt->_dt = DT_TYPE then
			if item.is_array = 0 then
				clrtext = "memset((void *)"
				clrtext += "&" & item.sym->ident
				clrtext += ", 0, "
				clrtext += "sizeof(" & dt->_sym->ident & ")"
				clrtext += ");"
				
				do_memset = -1
			else
				clrtext = "memset((void *)"
				clrtext += item.sym->ident
				clrtext += ", 0, "
				clrtext += "sizeof(" & dt->_sym->ident & ") * " & array_size
				clrtext += ");"
				do_memset = -1
			end if
		end if
	end if

	if item.init_expr then
		s += " = " & expr_to_str( item.init_expr )
	end if

	s += ";"

	emit_line( out_hfile, s )

	if do_memset then
		if is_shared then
			emit_line( out_hfile, "/* TODO, should be some code to clear this */" )
		else
			emit_line( out_hfile, clrtext )
		end if
	end if

	if is_shared = 0 then
		if dt->_dt = DT_STRING then
			emit_line( out_hfile, "strSetIsLocal(" & item.sym->ident & ");" )
		end if
	end if

end sub

'::::::::
sub emit_dim _
	( _
		byval out_hfile as FILE ptr, _
		byval node      as node_t ptr _
	)

	dim as list_dim_item_t ptr list

	list = node->dim_stmt->item_list
	while list
		emit_dim_item( out_hfile, node->dim_stmt->is_shared, list->item )
		list = list->_next
	wend

end sub

'::::::::
sub emit_extern _
	( _
		byval out_hfile as FILE ptr, _
		byval node as node_t ptr _
	)

	dim as string s
	dim as integer do_memset

	s = "extern " & datatype_to_str( @node->dt ) & " " & node->sym->ident

	if node->sym->is_array then
		s += "[" & node->sym->array_size & "]"
	end if

	s += ";"

	emit_line( out_hfile, s )

end sub

'::::::::
sub emit_type_decl _
	( _
		byval out_hfile as FILE ptr, _
		byval node as node_t ptr _
	)

	dim as string s
	dim as integer i

	emit_line( out_hfile, "/*::::::::*/" )
	emit_line( out_hfile, "struct " & node->sym->ident & !" {" )

	indent += 1

	i = 0
	while i < node->sym->member_count
		s = datatype_to_str( node->sym->members[i].dt ) & " " & node->sym->members[i].ident
		if node->sym->members[i].typ = SYM_VAR then
			if node->sym->members[i].is_array then
				s += "[" & node->sym->members[i].array_size & "]"
			end if
		end if
		s += ";"
		emit_line( out_hfile, s )
		i += 1
	wend

	indent -= 1

	emit_line( out_hfile, "};" )

end sub

sub emit_tree _
	( _
		byval out_hfile as FILE ptr, _
		byval node as node_t ptr _
	)

	dim as string s

	if node then
		if node->typ = NODE_BAD then
			die( "1" )
		' expression nodes
		elseif node->typ = NODE_LITINT then
			die( "2" )
		elseif node->typ = NODE_LITSTR then
			die( "3" )
		elseif node->typ = NODE_BOP then
			die( "4" )
		elseif node->typ = NODE_LVALUE then
			die( "5" )
		elseif node->typ = NODE_UOP then
			die( "6" )
		elseif node->typ = NODE_CONVERT then
			die( "7" )
		elseif node->typ = NODE_PROCCALL then
			emit_line( out_hfile, expr_to_str( node ) & ";" )
		elseif node->typ = NODE_ARRAY_ACCESS then
			die( "9" )
		elseif node->typ = NODE_VAR then
			die( "0" )
		elseif node->typ = NODE_PTR_ARRAY_ACCESS then
			die( "11" )
		elseif node->typ = NODE_ENUM_VAL then
			die( "NODE_ENUM_VAL" )
		' block/stmt nodes
		elseif node->typ = NODE_DIM then
			emit_dim( out_hfile, node )
		elseif node->typ = NODE_EXTERN then
			emit_extern( out_hfile, node )
		elseif node->typ = NODE_LABEL then
			emit_line( out_hfile, node->sym->ident & ":" )
		elseif node->typ = NODE_TYPE then
			die( "TYPE" )
		elseif node->typ = NODE_PROC then
			s = proc_header_to_str( node )
			emit_line( out_hfile, s )
			emit_line( out_hfile, "{" )
			indent += 1

			if node->sym->dt then
				dim as datatype_t ptr dt = node->sym->dt
				if dt->_dt = DT_STRING then
					emit_line( out_hfile, datatype_to_str( dt ) & " tb$result;" )
					emit_line( out_hfile, "strSetIsResult(tb$result);" )
				elseif (dt->_dt = DT_TYPE) and (dt->_ptr_cnt = 0) then
					emit_line( out_hfile, datatype_to_str( dt ) & " tb$result;" )
				else
					emit_line( out_hfile, datatype_to_str( dt ) & " tb$result = 0;" )
				end if
			end if

			emit_tree( out_hfile, node->stmt_list )

			dim as integer i = 0
			while i < node->sym->p_count
				dim as param_info_t ptr pi = node->sym->pi
				if (pi[i].is_byref) and (pi[i].dt->_dt = DT_STRING) and (pi[i].dt->_ptr_cnt = 0) then
					'emit_line( out_hfile, "mark_chain((TBSTRING *)&" & pi[i].ident & ");" )

					'emit_line( out_hfile, "if(" & pi[i].ident & ".is_result && " & pi[i].ident & ".old_temp) {" )
					'emit_line( out_hfile, "mark_chain(" & pi[i].ident & ".old_temp);" )
					'emit_line( out_hfile, "}" )

				end if
				i += 1
			wend

			if node->sym->ident = "MAIN" then
				'emit_line( out_hfile, "free_strings(1);" )
			else
				'emit_line( out_hfile, "free_strings(0);" )
			end if
			if node->sym->dt then
				emit_line( out_hfile, "return tb$result;" )
			end if
			indent -= 1
			emit_line( out_hfile, "}" )
		elseif node->typ = NODE_PROC_DECL then
			s = proc_header_to_str( node )
			emit_line( out_hfile, s & ";" )
		elseif node->typ = NODE_STMT_LIST then
			emit_tree( out_hfile, node->_data )
			emit_tree( out_hfile, node->_next )
		elseif node->typ = NODE_TYPE_DECL then
			emit_type_decl( out_hfile, node )
		elseif node->typ = NODE_TYPE_FWD_DECL then
			emit_line( out_hfile, "struct " & node->sym->_real & ";" )
		elseif node->typ = NODE_DUMMY then
			'emit_line( "/* " & node->dummy & " */" )
		elseif node->typ = NODE_ENUM then
			emit_enum( out_hfile, node )
		elseif node->typ = NODE_ASSIGN then
			dim as string l = expr_to_str( node->l )
			dim as string r = expr_to_str( node->r )
			s = l & " = " & r & ";"
			emit_line( out_hfile, s )
		elseif node->typ = NODE_IF then
			emit_if( out_hfile, node )
		elseif node->typ  = NODE_ELSEIF_LIST then
			emit_tree( out_hfile, node->_data )
			emit_tree( out_hfile, node->_next )
		elseif node->typ  = NODE_ELSEIF then
			emit_line( out_hfile, "} else if(" & expr_to_str( node->_expr ) & ") {" )
			indent += 1
			emit_tree( out_hfile, node->_stmt_list )
			indent -= 1
		elseif node->typ  = NODE_ELSE then
			emit_tree( out_hfile, node->_stmt_list )
		elseif node->typ = NODE_SELFADD then
			s = expr_to_str( node->l ) & " += " & expr_to_str( node->r ) & ";"
			emit_line( out_hfile, s )
		elseif node->typ = NODE_SELFSUB then
			s = expr_to_str( node->l ) & " -= " & expr_to_str( node->r ) & ";"
			emit_line( out_hfile, s )
		elseif node->typ = NODE_ASSIGN_RESULT then
			s = "tb$result = " & expr_to_str( node->l ) & ";"
			emit_line( out_hfile, s )
		elseif node->typ  = NODE_WHILE then
			emit_while( out_hfile, node )
		elseif node->typ  = NODE_RETURN then
			s = "tb$result = " & expr_to_str( node->_expr ) & ";"
			emit_line( out_hfile, s )
			emit_line( out_hfile, "return tb$result;" )
		elseif node->typ  = NODE_GOTO then
			emit_line( out_hfile, "goto " & node->_label & ";" )
		elseif node->typ  = NODE_BREAK then
			emit_line( out_hfile, "break;" )
		elseif node->typ  = NODE_PRINT then
			emit_line( out_hfile, "print(" & expr_to_str( node->_expr ) & ", " & node->_do_newline & ");" )
		elseif node->typ  = NODE_FOR then
			emit_line( out_hfile, "for(" & expr_to_str( node->index_expr ) & " = " & expr_to_str( node->from_expr ) & "; " & expr_to_str( node->index_expr ) & " <= " & expr_to_str( node->to_expr ) & "; " & expr_to_str( node->index_expr ) & "++) {" )
			indent += 1
			emit_tree( out_hfile, node->for_stmt_list )
			indent -= 1
			emit_line( out_hfile, "}" )
		else
			die( "Unknown node type in emit_tree A" )
		end if
	end if

end sub

sub destroy_tree _
	( _
		byval node as node_t ptr _
	)

	dim as string s

	if node then
		if node->typ = NODE_BAD then
			die( "1" )
		' expression nodes
		elseif node->typ = NODE_LITINT then
			node->_i = ""
		elseif node->typ = NODE_LITSTR then
			node->_s = ""
		elseif node->typ = NODE_BOP then
			node->_bop = ""
			destroy_tree( node->l )
			destroy_tree( node->r )
		elseif node->typ = NODE_LVALUE then
			die( "4" )
		elseif node->typ = NODE_UOP then
			node->_uop = ""
			destroy_tree( node->l )
		elseif node->typ = NODE_CONVERT then
			destroy_tree( node->l )
		elseif node->typ = NODE_PROCCALL then
			dim as integer i
			while i < node->expr_cnt
				destroy_tree( node->expr(i) )
				i += 1
			wend
		elseif node->typ = NODE_ARRAY_ACCESS then
			dim as integer i
			while i < node->expr_cnt
				destroy_tree( node->expr(i) )
				i += 1
			wend
		elseif node->typ = NODE_VAR then
			'die( "7" )
		elseif node->typ = NODE_PTR_ARRAY_ACCESS then
			dim as integer i
			while i < node->expr_cnt
				destroy_tree( node->expr(i) )
				i += 1
			wend
		elseif node->typ = NODE_ENUM_VAL then
			'die( "9" )
		' block/stmt nodes
		elseif node->typ = NODE_DIM then
			dim as list_dim_item_t ptr list

			list = node->dim_stmt->item_list
			while list
				if list->item.is_array then
					destroy_tree( list->item.bounds.lower_expr )
					destroy_tree( list->item.bounds.upper_expr )
				end if
				if list->item.init_expr then
					destroy_tree( list->item.init_expr )
				end if
				free( list->item.dt )
				dim as any ptr p_next = list->_next
				free( list )
				list = p_next
			wend

			free( node->dim_stmt )
		elseif node->typ = NODE_LABEL then
			'die( "11" )
		elseif node->typ = NODE_TYPE then
			'die( "12" )
		elseif node->typ = NODE_PROC then
			destroy_tree( node->stmt_list )
		elseif node->typ = NODE_PROC_DECL then
			'die( "13" )
		elseif node->typ = NODE_STMT_LIST then
			destroy_tree( node->_data )
			destroy_tree( node->_next )
		elseif node->typ = NODE_TYPE_DECL then
			'die( "14" )
		elseif node->typ = NODE_TYPE_FWD_DECL then
			'die( "14" )
		elseif node->typ = NODE_DUMMY then
			node->dummy = ""
		elseif node->typ = NODE_ENUM then
			'die( "16" )
		elseif node->typ = NODE_ASSIGN then
			destroy_tree( node->l )
			destroy_tree( node->r )
		elseif node->typ = NODE_IF then
			destroy_tree( node->if_expr )
			destroy_tree( node->if_stmt_list )
			destroy_tree( node->elseif_list )
			destroy_tree( node->else_node )
		elseif node->typ  = NODE_ELSEIF_LIST then
			destroy_tree( node->_data )
			destroy_tree( node->_next )
		elseif node->typ  = NODE_ELSEIF then
			destroy_tree( node->_expr )
			destroy_tree( node->_stmt_list )
		elseif node->typ  = NODE_ELSE then
			destroy_tree( node->_stmt_list )
		elseif node->typ = NODE_SELFADD then
			destroy_tree( node->l )
			destroy_tree( node->r )
		elseif node->typ = NODE_SELFSUB then
			destroy_tree( node->l )
			destroy_tree( node->r )
		elseif node->typ = NODE_ASSIGN_RESULT then
			destroy_tree( node->l )
		elseif node->typ  = NODE_WHILE then
			destroy_tree( node->_expr )
			destroy_tree( node->_stmt_list )
		elseif node->typ  = NODE_RETURN then
			destroy_tree( node->_expr )
		elseif node->typ  = NODE_GOTO then
			node->_label = ""
		elseif node->typ  = NODE_BREAK then
			'die( "22" )
		elseif node->typ  = NODE_PRINT then
			destroy_tree( node->_expr )
		elseif node->typ = NODE_EXTERN then
			'die( "23" )
		elseif node->typ  = NODE_FOR then
			destroy_tree( node->index_expr )
			destroy_tree( node->from_expr )
			destroy_tree( node->to_expr )
			destroy_tree( node->for_stmt_list )
		else
			die( "Unknown node type in emit_tree B" )
		end if
		if node->symtab then
			symtab_destroy( node->symtab )
			free( node->symtab )
		end if
		free( node )
	end if

end sub

