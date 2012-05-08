'::::::::
function parse_while _
	( _
	) as node_t ptr

	function = parse_expression( )

	if match( TK_EOL ) = 0 then
		expected( "end of line", tk_str )
	end if

end function

