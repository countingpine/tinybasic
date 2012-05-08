enum tk_e
	TK_BAD            = 0
	
	TK_CHAR_LPAREN    = asc( "(" )
	TK_CHAR_RPAREN    = asc( ")" )
	TK_CHAR_PLUS      = asc( "+" )
	TK_CHAR_MINUS     = asc( "-" )
	TK_CHAR_AT        = asc( "@" )
	TK_CHAR_ASTERISK  = asc( "*" )
	TK_CHAR_LSBRAC    = asc( "[" )
	TK_CHAR_RSBRAC    = asc( "]" )
	TK_CHAR_DOT       = asc( "." )
	TK_CHAR_COMMA     = asc( "," )
	TK_CHAR_EQ        = asc( "=" )
	TK_CHAR_LT        = asc( "<" )
	TK_CHAR_GT        = asc( ">" )
	TK_CHAR_HASH      = asc( "#" )
	TK_CHAR_COLON     = asc( ":" )
	TK_CHAR_SEMICOLON = asc( ";" )

	TK_IDENT          = 256
	TK_LITINT
	TK_EOF
	TK_LITSTR
	TK_EOL
	TK_SELFADD
	TK_NE
	TK_SELFSUB
	TK_ARROW
	TK_GE
	TK_DIM
	TK_SHARED
	TK_AS
	TK_INTEGER
	TK_STRING
	TK_TYPE
	TK_END
	TK_PTR
	TK_SUB
	TK_FUNCTION
	TK_BYVAL
	TK_BYREF
	TK_WHILE
	TK_WEND
	TK_IF
	TK_ELSEIF
	TK_RETURN
	TK_THEN
	TK_REM
	TK_DECLARE
	TK_GOTO
	TK_ELSE
	TK_DO
	TK_LOOP
	TK_EXIT
	TK_AND
	TK_OR
	TK_ANY
	TK_ZSTRING
	TK_ENUM
	TK_TO
	TK_INCLUDE
	TK_UNDEF
	TK_DEFINE
	TK_LE
	TK_UINTEGER
	TK_PRINT
	TK_ONCE
	TK_EXTERN
	TK_FOR
	TK_NEXT
end enum

type keyword_t
	s  as zstring ptr
	tk as integer
end type

extern as keyword_t keywords(0 to 511)
extern as integer keyword_count

'::::::::
declare function keyword_find _
	( _
		byref s as string _
	) as integer

