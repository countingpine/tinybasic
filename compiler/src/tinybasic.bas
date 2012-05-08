#undef rem
#define rem

rem #include once "crt.bi"
rem declare sub error_ cdecl alias "error" ( byval as integer, byval as integer, byval as zstring ptr, byval as zstring ptr )
rem declare sub error_at_line cdecl alias "error_at_line" _
	( _
		byval status   as integer, _
		byval errnum   as integer, _
		byval filename as zstring ptr, _
		byval linenum  as uinteger, _
		byval fmt      as zstring ptr, _
		byval s        as zstring ptr _
	)

declare sub read_char _
	( _
	)

declare sub read_token _
	( _
	)

rem declare function getcwd cdecl alias "getcwd" _
	( _
		byval buf  as zstring ptr, _
		byval size as integer _
	) as zstring ptr

#include once "compiler/src/inc/consts.bi"

#include once "compiler/src/inc/sym.bi"
#include once "compiler/src/inc/datatype.bi"
#include once "compiler/src/inc/help.bi"
#include once "compiler/src/inc/lex.bi"


#include once "compiler/src/inc/node.bi"

#include once "compiler/src/inc/tree.bi"

#include once "compiler/src/inc/keywords.bi"

'::::::::
declare function parse_stmt_list _
	( _
	) as node_t ptr

'::::::::
declare function parse_file _
	( _
	) as node_t ptr

declare function tree_node_proc _
	( _
		byval sym as sym_t ptr, _
		byval stmt_list as node_t ptr, _
		byval symtab as symtab_t ptr _
	) as node_t ptr

'type keyword_t
'	s  as zstring ptr
'	tk as integer
'end type



'dim shared as integer look
'dim shared as string  lexer_stack.curr_lex->tk_str
'dim shared as integer lexer_stack.curr_lex->tk_typ
dim shared as symstack_t symstack
dim shared as integer indent
'dim shared as integer curr_line
dim shared as keyword_t keywords(0 to 511)
dim shared as integer keyword_count

'dim shared as FILE ptr hfile_stk(0 to 15)
'dim shared as integer hfile_stk_ptr

'dim shared as integer inside_proc

#include once "compiler/src/inc/lex.bi"

dim shared as lexer_stack_t lexer_stack



#include once "compiler/src/keywords.bas"

'*******************************************************************************
' Emit
'*******************************************************************************



'*******************************************************************************
' Error
'*******************************************************************************

'*******************************************************************************
' Symbol table
'*******************************************************************************

#include once "compiler/src/sym.bas"

'*******************************************************************************
' Lexer
'*******************************************************************************

#include once "compiler/src/lex.bas"

'*******************************************************************************
' Tree
'*******************************************************************************

#include once "compiler/src/tree.bas"

'*******************************************************************************
' ???
'*******************************************************************************

#include once "compiler/src/help.bas"

'*******************************************************************************
' Parser
'*******************************************************************************

#include once "compiler/src/emit.bas"
#include once "compiler/src/parse.bas"
#include once "compiler/src/parse_extern.bas"
#include once "compiler/src/parse_pp.bas"
#include once "compiler/src/parse_for.bas"
#include once "compiler/src/parse_file.bas"

'*******************************************************************************
' Main
'*******************************************************************************

#include once "compiler/src/rtlib.bas"

sub rtlib_init _
	( _
	)

	'dim as datatype_t ptr dt_s = callocate( sizeof( datatype_t ) )
	dim as datatype_t ptr dt_i1 = callocate( sizeof( datatype_t ) )
	dim as datatype_t ptr dt_i2 = callocate( sizeof( datatype_t ) )
	dim as datatype_t ptr dt_i3 = callocate( sizeof( datatype_t ) )

	'dt_s->_dt = DT_STRING
	dt_i1->_dt = DT_INTEGER
	dt_i2->_dt = DT_INTEGER
	dt_i3->_dt = DT_INTEGER

	sym_add_type( "FILE", "FILE", 0, 0 )

	rtlib_add_lcase( )
	rtlib_add_ucase( )
	rtlib_add_exit_( )
	rtlib_add_asc( )
	rtlib_add_isdigit( )
	rtlib_add_isalnum( )
	rtlib_add_isalpha( )
	rtlib_add_callocate( )
	rtlib_add_sizeof( )
	rtlib_add_chr( )
	rtlib_add_val( )
	rtlib_add_TBSTRING_EQ( )
	rtlib_add_TBSTRING_NE( )
	rtlib_add_TBSTRING_assign( )
	rtlib_add_TBSTRING_append( )
	rtlib_add_tb_print( )
	rtlib_add_system_( )
	rtlib_add_tmpfile( )
	rtlib_add_fopen( )
	rtlib_add_fgetc( )
	rtlib_add_fflush( )
	rtlib_add_free( )
	rtlib_add_fputs( )
	rtlib_add_fclose( )
	rtlib_add_error_( )
	rtlib_add_error_at_line( )
	rtlib_add_getcwd( )

	'sym_add_dim( "__FUNCTION__", dt_s, 0, 0, -1 )
	sym_add_dim( "STDIN", "stdin", dt_i1, 0, 0, -1 )
	sym_add_dim( "STDOUT", "stdout", dt_i2, 0, 0, -1 )
	sym_add_dim( "EOF_", "EOF_", dt_i3, 0, 0, -1 )

end sub

type options_t
	compile_only as integer
	debug        as integer
	profile      as integer
	strip        as integer
	optimize     as integer
end type

sub compile_file _
	( _
		byref in_file_name as string, _
		byref out_file_name as string, _
		byref options as options_t _
	)

	dim as FILE ptr   in_hfile
	dim as FILE ptr   temp_hfile
	dim as string     temp_file_name = "_____temp.c"
	dim as node_t ptr tree

	symstack_init( )

	rtlib_init( )

	keywords_init( )

	if in_file_name = "-" then
		in_hfile = stdin
	else
		in_hfile = fopen( in_file_name, "rb" )
	end if

	temp_hfile = fopen( temp_file_name, "wb" )

	if in_hfile = 0 then
		die( "Could not open file '" & in_file_name & "' for input" )
	end if

	if temp_hfile = 0 then
		die( "Could not open file '" & temp_file_name & "' for output" )
	end if

	lexer_push( in_file_name, in_hfile )

	tree = parse_file( )

	emit_header( temp_hfile )
	emit_tree( temp_hfile, tree )

	destroy_tree( tree )

	fflush( temp_hfile )
	fclose( temp_hfile )

	if in_file_name <> "-" then
		fclose( in_hfile )
	end if

	dim as string flags

	if options.debug then
		flags += " -g "
	end if

	if options.profile then
		flags += " -p "
	end if

	if options.compile_only then
		flags += " -c "
	else
		flags += " o/*.o "
	end if

	if options.strip then
		flags += " -s "
	end if

	if options.optimize then
		flags += " -O2 "
	end if

	system_( "g++ " & flags & " " & temp_file_name & " -o " & out_file_name )

	lexer_pop( )

	if lexer_stack.curr_lex->tk_typ <> TK_EOF then
		die( "Lexer failed before end of file" )
	end if

	symstack_deinit( )

end sub

'::::::::
function main _
	( _
		byval argc as integer, _
		byval argv as zstring ptr ptr _
	) as integer

	dim as integer i
	dim as string in_file_name
	dim as string out_file_name

	dim as options_t options

	dim as integer a = 1, b = 2, c = 3

	options.compile_only = 0
	options.debug = 0
	options.profile = 0
	options.strip = 0
	options.optimize = 0

	'print "/* argc: " & argc & " */"

	i = 1
	while i < argc
		'print "/* argv: " & *argv[i] & " */"
		dim as string opt = *argv[i]
		if *argv[i] = "-c" then
			options.compile_only = -1
		elseif *argv[i] = "-g" then
			options.debug = -1
		elseif *argv[i] = "-p" then
			options.profile = -1
		elseif *argv[i] = "-s" then
			options.strip = -1
		elseif *argv[i] = "-O" then
			options.optimize = -1
		elseif opt = "-o" then
			if (i + 1) = argc then
				die( "No file specified for option -o" )
			end if
			i += 1
			out_file_name = *argv[i]
		else
			'print "/* !! in_file_name: " & in_file_name & " */"
			dim as integer j = (in_file_name <> "")
			if in_file_name <> "" then
				die( "Too many input files, cannot accept '" & in_file_name & "'" )
			end if
			in_file_name = *argv[i]
		end if
		i += 1
	wend

	'print "/* in_file_name: " & in_file_name & " */"
	'print "/* out_file_name: " & out_file_name & " */"

	if in_file_name = "" then
		die( "No input file" )
	end if

	if out_file_name = "" then
		die( "No output file" )
	end if

	compile_file( in_file_name, out_file_name, options )

	function = 0

end function

#include once "compiler/src/error.bas"
#include once "compiler/src/consts.bas"

'::::::::
rem main( __FB_ARGC__, __FB_ARGV__ )
