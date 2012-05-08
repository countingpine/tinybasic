#include once "compiler/src/inc/lex.bi"
#include once "compiler/src/inc/consts.bi"
#include once "compiler/src/inc/keywords.bi"

'::::::::
sub read_char _
	( _
	)

	lexer_stack.curr_lex->look = fgetc( lexer_stack.curr_lex->hfile )

end sub

'::::::::
sub skip_white _
	( _
	)

	while (lexer_stack.curr_lex->look = asc( " " )) or (lexer_stack.curr_lex->look = CHAR_TAB)
		read_char( )
	wend

end sub

'::::::::
function read_ident _
	( _
	) as string

	dim as string result

	while isalnum( lexer_stack.curr_lex->look ) or (lexer_stack.curr_lex->look = asc( "_" ))
		result += chr( lexer_stack.curr_lex->look )
		read_char( )
	wend

	function = result

end function

'::::::::
function read_litint _
	( _
	) as string

	dim as string result

	while isdigit( lexer_stack.curr_lex->look )
		result += chr( lexer_stack.curr_lex->look )
		read_char( )
	wend

	function = result

end function

'::::::::
function read_litstr _
	( _
	) as string

	dim as string result

	if lexer_stack.curr_lex->look <> CHAR_DBLQUOTE then
		print !"Expected '" & chr( CHAR_DBLQUOTE ) & "'"
		exit_( 1 )
	end if

	read_char( )

	while (lexer_stack.curr_lex->look <> EOF_) and (lexer_stack.curr_lex->look <> CHAR_DBLQUOTE)
		result += chr( lexer_stack.curr_lex->look )
		read_char( )
	wend

	if lexer_stack.curr_lex->look <> CHAR_DBLQUOTE then
		print !"Expected '" & chr( CHAR_DBLQUOTE ) & "'"
		exit_( 1 )
	end if

	read_char( )

	function = result

end function

sub skip_newlines _
	( _
	)

	while (lexer_stack.curr_lex->look = 13) or (lexer_stack.curr_lex->look = 10)
		if lexer_stack.curr_lex->look = 13 then
			read_char( )
			if lexer_stack.curr_lex->look = 10 then
				read_char( )
			end if
		elseif lexer_stack.curr_lex->look = 10 then
			read_char( )
		end if
		lexer_stack.curr_lex->curr_line += 1
		skip_white( )
	wend

end sub

'::::::::
sub read_token _
	( _
	)

read_token_start:

	skip_white( )

	lexer_stack.curr_lex->old_tk_str = lexer_stack.curr_lex->tk_str
	lexer_stack.curr_lex->old_tk_typ = lexer_stack.curr_lex->tk_typ

	lexer_stack.curr_lex->tk_str = "<<BAD>>"
	lexer_stack.curr_lex->tk_typ = TK_BAD

	dim as integer first_char = lexer_stack.curr_lex->look

	if first_char = EOF_ then
		lexer_stack.curr_lex->tk_str = "<<EOF>>"
		lexer_stack.curr_lex->tk_typ = TK_EOF
	elseif isalpha( first_char ) or (first_char = asc( "_" )) then
		lexer_stack.curr_lex->tk_str = ucase( read_ident( ) )
		lexer_stack.curr_lex->tk_typ = TK_IDENT
		if lexer_stack.curr_lex->tk_str = "_" then
			skip_newlines( )
			goto read_token_start
		end if
		dim as integer keyword_tk
		keyword_tk = keyword_find( lexer_stack.curr_lex->tk_str )
		if keyword_tk <> - 1 then
			lexer_stack.curr_lex->tk_typ = keyword_tk
		end if
	elseif isdigit( first_char ) then
		lexer_stack.curr_lex->tk_str = read_litint( )
		lexer_stack.curr_lex->tk_typ = TK_LITINT
	elseif first_char = CHAR_DBLQUOTE then
		lexer_stack.curr_lex->tk_str = read_litstr( )
		lexer_stack.curr_lex->tk_typ = TK_LITSTR
	elseif first_char = asc( "!" ) then
		read_char( )
		lexer_stack.curr_lex->tk_str = read_litstr( )
		lexer_stack.curr_lex->tk_typ = TK_LITSTR
	elseif (first_char = 13) or (first_char = 10) then
		skip_newlines( )
		lexer_stack.curr_lex->tk_str = "<<EOL>>"
		lexer_stack.curr_lex->tk_typ = TK_EOL
	elseif (first_char = asc( "'" )) then
		while (lexer_stack.curr_lex->look <> 10) and (lexer_stack.curr_lex->look <> 13) and (lexer_stack.curr_lex->look <> EOF_)
			read_char( )
		wend
		goto read_token_start
	elseif first_char = asc( "+" ) then
		lexer_stack.curr_lex->tk_str = chr( lexer_stack.curr_lex->look )
		lexer_stack.curr_lex->tk_typ = lexer_stack.curr_lex->look
		read_char( )
		if lexer_stack.curr_lex->look = asc( "=" ) then
			lexer_stack.curr_lex->tk_str = "+="
			lexer_stack.curr_lex->tk_typ = TK_SELFADD
			read_char( )
		end if
	elseif first_char = asc( "-" ) then
		lexer_stack.curr_lex->tk_str = chr( lexer_stack.curr_lex->look )
		lexer_stack.curr_lex->tk_typ = lexer_stack.curr_lex->look
		read_char( )
		if lexer_stack.curr_lex->look = asc( "=" ) then
			lexer_stack.curr_lex->tk_str = "-="
			lexer_stack.curr_lex->tk_typ = TK_SELFSUB
			read_char( )
		elseif lexer_stack.curr_lex->look = asc( ">" ) then
			lexer_stack.curr_lex->tk_str = "->"
			lexer_stack.curr_lex->tk_typ = TK_ARROW
			read_char( )
		end if
	elseif first_char = asc( "<" ) then
		lexer_stack.curr_lex->tk_str = chr( lexer_stack.curr_lex->look )
		lexer_stack.curr_lex->tk_typ = lexer_stack.curr_lex->look
		read_char( )
		if lexer_stack.curr_lex->look = asc( ">" ) then
			lexer_stack.curr_lex->tk_str = "<>"
			lexer_stack.curr_lex->tk_typ = TK_NE
			read_char( )
		elseif lexer_stack.curr_lex->look = asc( "=" ) then
			lexer_stack.curr_lex->tk_str = "<="
			lexer_stack.curr_lex->tk_typ = TK_LE
			read_char( )
		end if
	elseif first_char = asc( ">" ) then
		lexer_stack.curr_lex->tk_str = chr( lexer_stack.curr_lex->look )
		lexer_stack.curr_lex->tk_typ = lexer_stack.curr_lex->look
		read_char( )
		if lexer_stack.curr_lex->look = asc( "=" ) then
			lexer_stack.curr_lex->tk_str = ">="
			lexer_stack.curr_lex->tk_typ = TK_GE
			read_char( )
		end if
	else
		lexer_stack.curr_lex->tk_str = chr( lexer_stack.curr_lex->look )
		lexer_stack.curr_lex->tk_typ = lexer_stack.curr_lex->look
		read_char( )
	end if

	'print " // " & lexer_stack.curr_lex->tk_str

	''''print "<<" & lexer_stack.curr_lex->tk_str & ">>"

end sub

'::::::::
function match _
	( _
		byval t as integer _
	) as integer

	if lexer_stack.curr_lex->tk_typ = t then
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

	if ucase( lexer_stack.curr_lex->tk_str ) = ucase( s ) then
		function = -1
		read_token( )
	else
		function = 0
	end if

end function

sub lexer_push _
	( _
		byref file_name as string, _
		byval hfile     as FILE ptr _
	)

	if file_name = "stdin" then
		lexer_stack.curr_lex = 0
		lexer_stack.sp = 0
	end if

	if lexer_stack.sp = 16 then
		print "#include depth too deep (= 16)"
		exit_( 1 )
	else
		dim as lexer_t ptr lexer = @lexer_stack.lex(lexer_stack.sp)
		lexer->file_name = file_name
		lexer->hfile = hfile
		lexer->curr_line = 1
		lexer_stack.curr_lex = lexer
		lexer_stack.sp += 1
		read_char( )
		read_token( )
	end if

	'print "/* push: now in " & lexer_stack.curr_lex->file_name & " */"

end sub

function lexer_pop _
	( _
	) as FILE ptr

	if lexer_stack.sp = 0 then
		print "#include stack fault (= 0)"
		exit_( 1 )
	else
		lexer_stack.sp -= 1
		if lexer_stack.sp = 0 then
			lexer_stack.curr_lex = @lexer_stack.lex(0)
			'print "/* closed all files */"
		else
			'print "/* about to fclose " & lexer_stack.curr_lex->hfile & "*/"
			function = lexer_stack.curr_lex->hfile
			
			'print "/* switching to " & lexer_stack.sp - 1 & " */"
			lexer_stack.curr_lex = @lexer_stack.lex(lexer_stack.sp - 1)
		end if
	end if

	'print "/* pop: now in " & lexer_stack.curr_lex->file_name & " */"

end function

