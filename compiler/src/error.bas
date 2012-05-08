dim shared as integer curr_line

'::::::::
sub expected _
	( _
		byref what  as string, _
		byref found as string _
	)

	print "Expected : " & what
	print "Found    : " & found
	print "Line     : " & curr_line
	exit_( 1 )

end sub

