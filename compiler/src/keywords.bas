#include once "compiler/src/inc/keywords.bi"

sub keyword_add _
	( _
		byval s  as zstring ptr, _
		byval tk as integer _
	)

	keywords(keyword_count).s = s
	keywords(keyword_count).tk = tk

	keyword_count += 1

end sub

'::::::::
sub keywords_init _
	( _
	)

	keyword_count = 0

	keyword_add( "DIM",      TK_DIM )
	keyword_add( "SHARED",   TK_SHARED )
	keyword_add( "AS",       TK_AS )
	keyword_add( "INTEGER",  TK_INTEGER )
	keyword_add( "UINTEGER", TK_UINTEGER )
	keyword_add( "STRING",   TK_STRING )
	keyword_add( "TYPE",     TK_TYPE )
	keyword_add( "END",      TK_END )
	keyword_add( "PTR",      TK_PTR )
	keyword_add( "SUB",      TK_SUB )
	keyword_add( "FUNCTION", TK_FUNCTION )
	keyword_add( "BYVAL",    TK_BYVAL )
	keyword_add( "BYREF",    TK_BYREF )
	keyword_add( "WHILE",    TK_WHILE )
	keyword_add( "WEND",     TK_WEND )
	keyword_add( "IF",       TK_IF )
	keyword_add( "ELSEIF",   TK_ELSEIF)
	keyword_add( "RETURN",   TK_RETURN )
	keyword_add( "THEN",     TK_THEN )
	keyword_add( "REM",      TK_REM )
	keyword_add( "DECLARE",  TK_DECLARE )
	keyword_add( "GOTO",     TK_GOTO )
	keyword_add( "ELSE",     TK_ELSE )
	keyword_add( "DO",       TK_DO )
	keyword_add( "LOOP",     TK_LOOP )
	keyword_add( "EXIT",     TK_EXIT )
	keyword_add( "AND",      TK_AND )
	keyword_add( "OR",       TK_OR )
	keyword_add( "ANY",      TK_ANY )
	keyword_add( "ZSTRING",  TK_ZSTRING )
	keyword_add( "ENUM",     TK_ENUM )
	keyword_add( "TO",       TK_TO )
	keyword_add( "INCLUDE",  TK_INCLUDE )
	keyword_add( "UNDEF",    TK_UNDEF )
	keyword_add( "DEFINE",   TK_DEFINE )
	keyword_add( "PRINT",    TK_PRINT )
	keyword_add( "ONCE",     TK_ONCE )
	keyword_add( "EXTERN",   TK_EXTERN )
	keyword_add( "FOR",      TK_FOR )
	keyword_add( "NEXT",     TK_NEXT )

end sub

'::::::::
function keyword_find _
	( _
		byref s as string _
	) as integer

	dim as integer i

	function = -1

	for i = 0 to keyword_count - 1
		if ucase( s ) = *keywords(i).s then
			function = keywords(i).tk
		end if
	next i

end function
