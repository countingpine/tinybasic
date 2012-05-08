'::::::::
function parse_datatype _
	( _
	) as datatype_t ptr

	dim as datatype_t ptr datatype = callocate( sizeof( datatype_t ) )
	dim as string dt = tk_str
	dim as integer ptr_cnt
	dim as integer i
	dim as string result

	if lcase( dt ) = "string" then
		dt = "FBSTRING"
	elseif lcase( dt ) = "any" then
		dt = "void"
	elseif lcase( dt ) = "integer" then
		dt = "integer"
	elseif lcase( dt ) = "zstring" then
		dt = "zstring"
	end if

	read_token( )

	while lcase( tk_str ) = "ptr"
		ptr_cnt += 1
		read_token( )
	wend

	result = dt

	i = 0
	while i < ptr_cnt
		result += " *"
		i += 1
	wend       

	datatype->dt = sym_find( dt, 0 )
	datatype->ptr_cnt = ptr_cnt
	datatype->s = result

	function = datatype
       
end function

