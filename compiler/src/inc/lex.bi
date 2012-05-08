type lexer_t
	file_name  as string
	hfile      as FILE ptr
	look       as integer
	tk_typ     as integer
	tk_str     as string
	old_tk_typ as integer
	old_tk_str as string
	curr_line  as integer
end type

type lexer_stack_t
	lex(0 to 31) as lexer_t
	sp           as integer
	curr_lex     as lexer_t ptr
end type

extern as lexer_stack_t lexer_stack
