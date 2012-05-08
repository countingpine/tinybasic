sub rtlib_add_lcase _
	( _
	)

	dim as param_info_t ptr pi = callocate( sizeof( param_info_t ) * 16 )
	dim as datatype_t ptr dt = callocate( sizeof( datatype_t ) )
	dim as datatype_t ptr dt1 = callocate( sizeof( datatype_t ) )

	dt->_dt = DT_STRING

	dt1->_dt = DT_STRING

	pi[0].dt = dt1

	sym_add_proc( "LCASE", "tb$lcase", dt, pi, 1 )

end sub

sub rtlib_add_ucase _
	( _
	)

	dim as param_info_t ptr pi = callocate( sizeof( param_info_t ) * 16 )
	dim as datatype_t ptr dt = callocate( sizeof( datatype_t ) )
	dim as datatype_t ptr dt1 = callocate( sizeof( datatype_t ) )

	dt->_dt = DT_STRING

	dt1->_dt = DT_STRING

	pi[0].dt = dt1

	sym_add_proc( "UCASE", "tb$ucase", dt, pi, 1 )

end sub

sub rtlib_add_exit_ _
	( _
	)

	dim as param_info_t ptr pi = callocate( sizeof( param_info_t ) * 16 )
	dim as datatype_t ptr dt = callocate( sizeof( datatype_t ) )
	dim as datatype_t ptr dt1 = callocate( sizeof( datatype_t ) )

	dt->_dt = DT_ANY

	dt1->_dt = DT_INTEGER

	pi[0].dt = dt1

	sym_add_proc( "EXIT_", "exit_", dt, pi, 1 )

end sub

sub rtlib_add_fgetc _
	( _
	)

	dim as param_info_t ptr pi = callocate( sizeof( param_info_t ) * 16 )
	dim as datatype_t ptr dt = callocate( sizeof( datatype_t ) )
	dim as datatype_t ptr dt1 = callocate( sizeof( datatype_t ) )

	dt->_dt = DT_INTEGER

	dt1->_dt = DT_TYPE
	dt1->_ptr_cnt = 1
	dt1->_sym = sym_find( "FILE", 0 )

	pi[0].dt = dt1

	sym_add_proc( "FGETC", "fgetc", dt, pi, 1 )

end sub

sub rtlib_add_fflush _
	( _
	)

	dim as param_info_t ptr pi = callocate( sizeof( param_info_t ) * 16 )
	dim as datatype_t ptr dt = callocate( sizeof( datatype_t ) )
	dim as datatype_t ptr dt1 = callocate( sizeof( datatype_t ) )

	dt->_dt = DT_INTEGER

	dt1->_dt = DT_TYPE
	dt1->_ptr_cnt = 1
	dt1->_sym = sym_find( "FILE", 0 )

	pi[0].dt = dt1

	sym_add_proc( "FFLUSH", "fflush", dt, pi, 1 )

end sub

sub rtlib_add_free _
	( _
	)

	dim as param_info_t ptr pi = callocate( sizeof( param_info_t ) * 16 )
	dim as datatype_t ptr dt = callocate( sizeof( datatype_t ) )
	dim as datatype_t ptr dt1 = callocate( sizeof( datatype_t ) )

	dt->_dt = DT_INTEGER

	dt1->_dt = DT_ANY
	dt1->_ptr_cnt = 1

	pi[0].dt = dt1

	sym_add_proc( "FREE", "free", dt, pi, 1 )

end sub

sub rtlib_add_fputs _
	( _
	)

	dim as param_info_t ptr pi = callocate( sizeof( param_info_t ) * 16 )
	dim as datatype_t ptr dt = callocate( sizeof( datatype_t ) )
	dim as datatype_t ptr dt1 = callocate( sizeof( datatype_t ) )
	dim as datatype_t ptr dt2 = callocate( sizeof( datatype_t ) )

	dt->_dt = DT_INTEGER

	dt1->_dt = DT_ZSTRING
	dt1->_ptr_cnt = 1

	dt2->_dt = DT_TYPE
	dt2->_ptr_cnt = 1
	dt2->_sym = sym_find( "FILE", 0 )

	pi[0].dt = dt1
	pi[1].dt = dt2

	sym_add_proc( "FPUTS", "fputs", dt, pi, 2 )

end sub

sub rtlib_add_system_ _
	( _
	)

	dim as param_info_t ptr pi = callocate( sizeof( param_info_t ) * 16 )
	dim as datatype_t ptr dt = callocate( sizeof( datatype_t ) )
	dim as datatype_t ptr dt1 = callocate( sizeof( datatype_t ) )

	dt->_dt = DT_INTEGER

	dt1->_dt = DT_ZSTRING
	dt1->_ptr_cnt = 1

	pi[0].dt = dt1

	sym_add_proc( "SYSTEM_", "system_", dt, pi, 1 )

end sub

sub rtlib_add_tmpfile _
	( _
	)

	dim as param_info_t ptr pi = callocate( sizeof( param_info_t ) * 16 )
	dim as datatype_t ptr dt = callocate( sizeof( datatype_t ) )

	dt->_dt = DT_TYPE
	dt->_ptr_cnt = 1
	dt->_sym = sym_find( "FILE", 0 )

	sym_add_proc( "TMPFILE", "tmpfile", dt, pi, 0 )

end sub

sub rtlib_add_fopen _
	( _
	)

	dim as param_info_t ptr pi = callocate( sizeof( param_info_t ) * 16 )
	dim as datatype_t ptr dt = callocate( sizeof( datatype_t ) )
	dim as datatype_t ptr dt1 = callocate( sizeof( datatype_t ) )
	dim as datatype_t ptr dt2 = callocate( sizeof( datatype_t ) )

	dt->_dt = DT_TYPE
	dt->_ptr_cnt = 1
	dt->_sym = sym_find( "FILE", 0 )

	dt1->_dt = DT_ZSTRING
	dt1->_ptr_cnt = 1

	dt2->_dt = DT_ZSTRING
	dt2->_ptr_cnt = 1

	pi[0].dt = dt1
	pi[1].dt = dt2

	sym_add_proc( "FOPEN", "fopen", dt, pi, 2 )

end sub

sub rtlib_add_fclose _
	( _
	)

	dim as param_info_t ptr pi = callocate( sizeof( param_info_t ) * 16 )
	dim as datatype_t ptr dt = callocate( sizeof( datatype_t ) )
	dim as datatype_t ptr dt1 = callocate( sizeof( datatype_t ) )

	dt->_dt = DT_INTEGER

	dt1->_dt = DT_TYPE
	dt1->_ptr_cnt = 1
	dt1->_sym = sym_find( "FILE", 0 )

	pi[0].dt = dt1

	sym_add_proc( "FCLOSE", "fclose", dt, pi, 1 )

end sub

sub rtlib_add_asc _
	( _
	)

	dim as param_info_t ptr pi = callocate( sizeof( param_info_t ) * 16 )
	dim as datatype_t ptr dt = callocate( sizeof( datatype_t ) )
	dim as datatype_t ptr dt1 = callocate( sizeof( datatype_t ) )

	dt->_dt = DT_INTEGER

	dt1->_dt = DT_STRING

	pi[0].dt = dt1

	sym_add_proc( "ASC", "tb$asc", dt, pi, 1 )

end sub

sub rtlib_add_isdigit _
	( _
	)

	dim as param_info_t ptr pi = callocate( sizeof( param_info_t ) * 16 )
	dim as datatype_t ptr dt = callocate( sizeof( datatype_t ) )
	dim as datatype_t ptr dt1 = callocate( sizeof( datatype_t ) )

	dt->_dt = DT_INTEGER

	dt1->_dt = DT_INTEGER

	pi[0].dt = dt1

	sym_add_proc( "ISDIGIT", "isdigit", dt, pi, 1 )

end sub

sub rtlib_add_isalnum _
	( _
	)

	dim as param_info_t ptr pi = callocate( sizeof( param_info_t ) * 16 )
	dim as datatype_t ptr dt = callocate( sizeof( datatype_t ) )
	dim as datatype_t ptr dt1 = callocate( sizeof( datatype_t ) )

	dt->_dt = DT_INTEGER

	dt1->_dt = DT_INTEGER

	pi[0].dt = dt1

	sym_add_proc( "ISALNUM", "isalnum", dt, pi, 1 )

end sub

sub rtlib_add_isalpha _
	( _
	)

	dim as param_info_t ptr pi = callocate( sizeof( param_info_t ) * 16 )
	dim as datatype_t ptr dt = callocate( sizeof( datatype_t ) )
	dim as datatype_t ptr dt1 = callocate( sizeof( datatype_t ) )

	dt->_dt = DT_INTEGER

	dt1->_dt = DT_INTEGER

	pi[0].dt = dt1

	sym_add_proc( "ISALPHA", "isalpha", dt, pi, 1 )

end sub

sub rtlib_add_callocate _
	( _
	)

	dim as param_info_t ptr pi = callocate( sizeof( param_info_t ) * 16 )
	dim as datatype_t ptr dt = callocate( sizeof( datatype_t ) )
	dim as datatype_t ptr dt1 = callocate( sizeof( datatype_t ) )

	dt->_dt = DT_ANY
	dt->_ptr_cnt = 1

	dt1->_dt = DT_INTEGER

	pi[0].dt = dt1

	sym_add_proc( "CALLOCATE", "callocate", dt, pi, 1 )

end sub

sub rtlib_add_sizeof _
	( _
	)

	dim as param_info_t ptr pi = callocate( sizeof( param_info_t ) * 16 )
	dim as datatype_t ptr dt = callocate( sizeof( datatype_t ) )
	dim as datatype_t ptr dt1 = callocate( sizeof( datatype_t ) )

	dt->_dt = DT_INTEGER

	dt1->_dt = DT_ANY

	pi[0].dt = dt1

	sym_add_proc( "SIZEOF", "sizeof", dt, pi, 1 )

end sub

sub rtlib_add_chr _
	( _
	)

	dim as param_info_t ptr pi = callocate( sizeof( param_info_t ) * 16 )
	dim as datatype_t ptr dt = callocate( sizeof( datatype_t ) )
	dim as datatype_t ptr dt1 = callocate( sizeof( datatype_t ) )

	dt->_dt = DT_STRING

	dt1->_dt = DT_INTEGER

	pi[0].dt = dt1

	sym_add_proc( "CHR", "tb$chr", dt, pi, 1 )

end sub

sub rtlib_add_val _
	( _
	)

	dim as param_info_t ptr pi = callocate( sizeof( param_info_t ) * 16 )
	dim as datatype_t ptr dt = callocate( sizeof( datatype_t ) )
	dim as datatype_t ptr dt1 = callocate( sizeof( datatype_t ) )

	dt->_dt = DT_INTEGER

	dt1->_dt = DT_STRING

	pi[0].dt = dt1

	sym_add_proc( "VAL", "tb$val", dt, pi, 1 )

end sub

sub rtlib_add_TBSTRING_EQ _
	( _
	)

	dim as param_info_t ptr pi = callocate( sizeof( param_info_t ) * 16 )
	dim as datatype_t ptr dt = callocate( sizeof( datatype_t ) )
	dim as datatype_t ptr dt1 = callocate( sizeof( datatype_t ) )
	dim as datatype_t ptr dt2 = callocate( sizeof( datatype_t ) )

	dt->_dt = DT_INTEGER

	dt1->_dt = DT_STRING
	dt2->_dt = DT_STRING

	pi[0].dt = dt1
	pi[1].dt = dt2

	sym_add_proc( "TBSTRING_EQ", "TBSTRING_EQ", dt, pi, 2 )

end sub

sub rtlib_add_TBSTRING_NE _
	( _
	)

	dim as param_info_t ptr pi = callocate( sizeof( param_info_t ) * 16 )
	dim as datatype_t ptr dt = callocate( sizeof( datatype_t ) )
	dim as datatype_t ptr dt1 = callocate( sizeof( datatype_t ) )
	dim as datatype_t ptr dt2 = callocate( sizeof( datatype_t ) )

	dt->_dt = DT_INTEGER

	dt1->_dt = DT_STRING
	dt2->_dt = DT_STRING

	pi[0].dt = dt1
	pi[1].dt = dt2

	sym_add_proc( "TBSTRING_NE", "TBSTRING_NE", dt, pi, 2 )

end sub

sub rtlib_add_TBSTRING_assign _
	( _
	)

	dim as param_info_t ptr pi = callocate( sizeof( param_info_t ) * 16 )
	dim as datatype_t ptr dt = callocate( sizeof( datatype_t ) )
	dim as datatype_t ptr dt1 = callocate( sizeof( datatype_t ) )
	dim as datatype_t ptr dt2 = callocate( sizeof( datatype_t ) )

	dt->_dt = DT_INTEGER

	dt1->_dt = DT_STRING
	dt2->_dt = DT_STRING

	pi[0].dt = dt1
	pi[1].dt = dt2

	sym_add_proc( "TBSTRING_ASSIGN", "TBSTRING_assign", dt, pi, 2 )

end sub

sub rtlib_add_TBSTRING_append _
	( _
	)

	dim as param_info_t ptr pi = callocate( sizeof( param_info_t ) * 16 )
	dim as datatype_t ptr dt = callocate( sizeof( datatype_t ) )
	dim as datatype_t ptr dt1 = callocate( sizeof( datatype_t ) )
	dim as datatype_t ptr dt2 = callocate( sizeof( datatype_t ) )

	dt->_dt = DT_INTEGER

	dt1->_dt = DT_STRING
	dt2->_dt = DT_STRING

	pi[0].dt = dt1
	pi[1].dt = dt2

	sym_add_proc( "TBSTRING_APPEND", "TBSTRING_append", dt, pi, 2 )

end sub

sub rtlib_add_tb_print _
	( _
	)

	dim as param_info_t ptr pi = callocate( sizeof( param_info_t ) * 16 )
	dim as datatype_t ptr dt = callocate( sizeof( datatype_t ) )
	dim as datatype_t ptr dt1 = callocate( sizeof( datatype_t ) )
	dim as datatype_t ptr dt2 = callocate( sizeof( datatype_t ) )

	dt->_dt = DT_ANY

	dt1->_dt = DT_STRING
	dt2->_dt = DT_INTEGER

	pi[0].dt = dt1
	pi[1].dt = dt2

	sym_add_proc( "TB_PRINT", "tb$print", dt, pi, 2 )

end sub

sub rtlib_add_error_ _
	( _
	)

	dim as param_info_t ptr pi = callocate( sizeof( param_info_t ) * 16 )
	dim as datatype_t ptr dt = callocate( sizeof( datatype_t ) )
	dim as datatype_t ptr dt1 = callocate( sizeof( datatype_t ) )
	dim as datatype_t ptr dt2 = callocate( sizeof( datatype_t ) )
	dim as datatype_t ptr dt3 = callocate( sizeof( datatype_t ) )
	dim as datatype_t ptr dt4 = callocate( sizeof( datatype_t ) )

	dt->_dt = DT_ANY

	dt1->_dt = DT_INTEGER

	dt2->_dt = DT_INTEGER

	dt3->_dt = DT_ZSTRING
	dt3->_ptr_cnt = 1

	dt4->_dt = DT_ZSTRING
	dt4->_ptr_cnt = 1

	pi[0].dt = dt1
	pi[1].dt = dt2
	pi[2].dt = dt3
	pi[3].dt = dt4

	sym_add_proc( "ERROR_", "error_", dt, pi, 4 )

end sub

sub rtlib_add_error_at_line _
	( _
	)

	dim as param_info_t ptr pi = callocate( sizeof( param_info_t ) * 16 )
	dim as datatype_t ptr dt = callocate( sizeof( datatype_t ) )
	dim as datatype_t ptr dt1 = callocate( sizeof( datatype_t ) )
	dim as datatype_t ptr dt2 = callocate( sizeof( datatype_t ) )
	dim as datatype_t ptr dt3 = callocate( sizeof( datatype_t ) )
	dim as datatype_t ptr dt4 = callocate( sizeof( datatype_t ) )
	dim as datatype_t ptr dt5 = callocate( sizeof( datatype_t ) )
	dim as datatype_t ptr dt6 = callocate( sizeof( datatype_t ) )

	dt->_dt = DT_ANY

	dt1->_dt = DT_INTEGER

	dt2->_dt = DT_INTEGER

	dt3->_dt = DT_ZSTRING
	dt3->_ptr_cnt = 1

	dt4->_dt = DT_UINTEGER

	dt5->_dt = DT_ZSTRING
	dt5->_ptr_cnt = 1

	dt6->_dt = DT_ZSTRING
	dt6->_ptr_cnt = 1

	pi[0].dt = dt1
	pi[1].dt = dt2
	pi[2].dt = dt3
	pi[3].dt = dt4
	pi[4].dt = dt5
	pi[5].dt = dt6

	sym_add_proc( "ERROR_AT_LINE", "error_at_line", dt, pi, 6 )

end sub

sub rtlib_add_getcwd _
	( _
	)

	dim as param_info_t ptr pi = callocate( sizeof( param_info_t ) * 16 )
	dim as datatype_t ptr dt = callocate( sizeof( datatype_t ) )
	dim as datatype_t ptr dt1 = callocate( sizeof( datatype_t ) )
	dim as datatype_t ptr dt2 = callocate( sizeof( datatype_t ) )

	dt->_dt = DT_ZSTRING
	dt->_ptr_cnt = 1

	dt1->_dt = DT_ZSTRING
	dt1->_ptr_cnt = 1

	dt2->_dt = DT_INTEGER

	pi[0].dt = dt1
	pi[1].dt = dt2

	sym_add_proc( "GETCWD", "getcwd", dt, pi, 2 )

end sub
