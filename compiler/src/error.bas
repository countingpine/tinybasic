#include once "compiler/src/inc/lex.bi"

'::::::::
sub error_at_line2 _
	( _
		byval status   as integer, _
		byval errnum   as integer, _
		byval filename as zstring ptr, _
		byval linenum  as integer, _
		byval fmt      as zstring ptr, _
		byval s        as zstring ptr _
	)

	dim as string _s = *s

	print "filename: " & *filename
	print "line    : " & linenum
	print _s

	exit_( status )

end sub

'::::::::
sub expected _
	( _
		byref what  as string, _
		byref found as string _
	)

	dim as string  s = "Expected " & what & ", found " & found
	dim as string  curr_file
	dim as integer curr_line

	if lexer_stack.curr_lex then
		curr_file = lexer_stack.curr_lex->file_name
		curr_line = lexer_stack.curr_lex->curr_line
	end if

	error_at_line2( 1, 0, curr_file, curr_line, "%s", s )

end sub

'::::::::
sub die _
	( _
		byref why as string _
	)

	dim as string  curr_file
	dim as integer curr_line

	if lexer_stack.curr_lex then
		curr_file = lexer_stack.curr_lex->file_name
		curr_line = lexer_stack.curr_lex->curr_line
	end if

	error_at_line2( 1, 0, curr_file, curr_line, "%s", why )

end sub
