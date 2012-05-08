#include once "inc/datatype.bi"

'::::::::
function get_dt_size _
	( _
		byval dt as datatype_t ptr _
	) as integer

	if dt->ptr_cnt > 0 then
		function = 4
	elseif dt->s = "integer" then
		function = 4
	else
		function = 0
	end if

end function

