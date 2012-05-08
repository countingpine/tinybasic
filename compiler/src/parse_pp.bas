dim shared as string included(0 to 63)
dim shared as integer included_count

sub add_included _
	( _
		byref file_name as string _
	)

	'print "adding " & file_name

	included(included_count) = file_name
	included_count += 1

end sub

function has_included _
	( _
		byref file_name as string _
	) as integer

	dim as integer i
	
	while i < included_count
		if included(i) = file_name then
			return -1
		end if
		i += 1
	wend

	return 0

end function

function pp_include_check _
	( _
		byref file_name as string _
	) as integer

	dim as string s
	dim as zstring ptr tmp = callocate( 4096 )

	if getcwd( tmp, 4095 ) = 0 then
		die( "getcwd failed" )
	end if

	s = *tmp & "/"

	free(tmp)

	if has_included( s & file_name ) then
		return 1
	end if

	if has_included( s & "src/" & file_name ) then
		return 1
	end if

	if has_included( s & "src/inc/" & file_name ) then
		return 1
	end if

	return 0

end function

function pp_include_file_open _
	( _
		byref file_name as string _
	) as FILE ptr

	dim as FILE ptr hfile
	dim as string s
	dim as zstring ptr tmp = callocate( 4096 )

	if getcwd( tmp, 4095 ) = 0 then
		die( "getcwd failed" )
	end if

	s = *tmp & "/"

	free(tmp)

	hfile = fopen( file_name, "rb" )
	if hfile then
		add_included( s & file_name )
		return hfile
	end if

	die( "file not found '" & file_name & "'" )

end function

function parse_pp_include _
	( _
	) as node_t ptr

	dim as string file_name
	dim as FILE ptr hfile
	dim as integer is_once

	if match( TK_ONCE ) then
		is_once = -1
	end if

	file_name = lexer_stack.curr_lex->tk_str

	if match( TK_LITSTR ) = 0 then
		expected( "literal string", lexer_stack.curr_lex->tk_str )
	end if

	if match( TK_EOL ) = 0 then
		expected( "end of line", lexer_stack.curr_lex->tk_str )
	end if

	if (is_once = -1) and pp_include_check( file_name ) then
		function = tree_node_dummy( "already included!" )
	else
		hfile = pp_include_file_open( file_name )

		lexer_push( file_name, hfile )
	
		function = parse_file( )
	
		fclose( lexer_pop( ) )
	end if

end function

function parse_pp_undef _
	( _
	) as node_t ptr

	while (lexer_stack.curr_lex->tk_typ <> TK_EOL) _
	  and (lexer_stack.curr_lex->tk_typ <> TK_EOF)
		read_token( )
	wend

	if match( TK_EOL ) = 0 then
		expected( "end of line", lexer_stack.curr_lex->tk_str )
	end if

	function = tree_node_dummy( "parse_pp_undef" )

end function

function parse_pp_define _
	( _
	) as node_t ptr

	while (lexer_stack.curr_lex->tk_typ <> TK_EOL) _
	  and (lexer_stack.curr_lex->tk_typ <> TK_EOF)
		read_token( )
	wend

	if match( TK_EOL ) = 0 then
		expected( "end of line", lexer_stack.curr_lex->tk_str )
	end if

	function = tree_node_dummy( "parse_pp_define" )

end function

function parse_pp _
	( _
	) as node_t ptr

	if match( TK_INCLUDE ) then
		function = parse_pp_include( )
	elseif match( TK_UNDEF ) then
		function = parse_pp_undef( )
	elseif match( TK_DEFINE ) then
		function = parse_pp_define( )
	else
		expected( "Preprocessor statement", lexer_stack.curr_lex->tk_str )
	end if

end function
