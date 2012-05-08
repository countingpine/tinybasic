'::::::::
function new_datatype _
	( _
		byval dt      as integer, _
		byval ptr_cnt as integer, _
		byval sym     as sym_t ptr _
	) as datatype_t ptr

	dim as datatype_t ptr datatype = callocate( sizeof( datatype_t ) )

	datatype->_dt      = dt
	datatype->_ptr_cnt = ptr_cnt
	datatype->_sym     = sym

	function = datatype

end function

'::::::::
function parse_datatype _
	( _
	) as datatype_t ptr

	dim as integer   dt
	dim as integer   ptr_cnt
	dim as sym_t ptr sym

	sym = sym_find( lexer_stack.curr_lex->tk_str, 0 )

	if sym then
		dt = DT_TYPE
	else
		dim as integer typ = lexer_stack.curr_lex->tk_typ

		if typ = TK_STRING then
			dt = DT_STRING
		elseif typ = TK_ANY then
			dt = DT_ANY
		elseif typ = TK_INTEGER then
			dt = DT_INTEGER
		elseif typ = TK_UINTEGER then
			dt = DT_UINTEGER
		elseif typ = TK_ZSTRING then
			dt = DT_ZSTRING
		else
			expected( "datatype", "'" & lexer_stack.curr_lex->tk_str & "'" )
		end if
	end if

	read_token( )

	while lexer_stack.curr_lex->tk_typ = TK_PTR
		ptr_cnt += 1
		read_token( )
	wend

	function = new_datatype( dt, ptr_cnt, sym )
	
end function
