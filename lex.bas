#include once "inc/consts.bi"
#include once "inc/keywords.bi"

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


