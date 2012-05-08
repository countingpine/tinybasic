#include once "compiler/src/inc/datatype.bi"

'::::::::
function get_dt_size _
	( _
		byval dt as datatype_t ptr _
	) as integer

	dim as integer i

	if dt->_ptr_cnt > 0 then
		function = 4
	elseif dt->_dt = DT_INTEGER then
		function = 4
	elseif dt->_dt = DT_TYPE then
		die( "can't size type yet" )
	else
		function = 0
	end if

end function
