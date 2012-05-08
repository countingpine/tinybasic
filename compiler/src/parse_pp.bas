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

