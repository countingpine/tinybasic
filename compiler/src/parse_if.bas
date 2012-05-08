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

