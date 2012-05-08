'::::::::
sub parse_file _
	( _
	)

	moop:
	while tk_typ <> TK_EOF
		if tk_str = "#" then
			read_token( )
			if lcase( tk_str ) = "include" then
				read_token( )
				parse_pp_include( )
			elseif lcase( tk_str ) = "undef" then
				parse_pp_dummy( )
			elseif lcase( tk_str ) = "define" then
				parse_pp_dummy( )
			else
				print "Expected preprocessor command"
				exit_( 1 )
			end if
		elseif lcase( tk_str ) = "rem" then
			parse_pp_dummy( )
		elseif match_str( "dim" ) then
			dim as node_t ptr node
			dim as string s
			node = parse_dim( )
			s = dim_to_str( node )
			emit_line( s )
		elseif lcase( tk_str ) = "type" then
			read_token( )
			parse_type( )
		elseif (lcase( tk_str ) = "declare") then
			read_token( )
			read_token( )
			parse_sub( -1 )
		elseif (lcase( tk_str ) = "sub") or lcase( tk_str ) = "function" then
			dim as integer is_func = lcase( tk_str ) = "function"
			dim as node_t ptr expr
			read_token( )
			if is_func then
				if tk_str = "=" then
					if match_str( "=" ) = 0 then
						expected( "'='", tk_str )
					end if
					expr = parse_expression( )
					if match( TK_EOL ) = 0 then
						expected( "end of line", tk_str )
					end if
					emit_line( "func$result = " & expr_to_str( expr ) & ";" )
				else
					parse_sub( 0 )
				end if
			else
				parse_sub( 0 )
			end if
		elseif lcase( tk_str ) = "print" then
			dim as node_t ptr expr
			dim as integer has_semi
			read_token( )
			expr = parse_expression( )
			has_semi = match_str( ";" )
			if match( TK_EOL ) = 0 then
				expected( "end of line", tk_str )
			end if
			if has_semi then
				emit_line( "print(" & expr_to_str( expr ) & ", 0);" )
			else
				emit_line( "print(" & expr_to_str( expr ) & ", 1);" )
			end if
		elseif lcase( tk_str ) = "return" then
			dim as node_t ptr expr
			read_token( )
			expr = parse_expression( )
			if match( TK_EOL ) = 0 then
				expected( "end of line", tk_str )
			end if
			emit_line( "return(" & expr_to_str( expr ) & ");" )
		elseif lcase( tk_str ) = "goto" then
			'dim as node_t ptr expr
			dim as string s
			read_token( )
			s = tk_str
			read_token( )
			'expr = parse_expression( )
			if match( TK_EOL ) = 0 then
				expected( "end of line", tk_str )
			end if
			emit_line( "goto " & s & ";" )
		elseif lcase( tk_str ) = "while" then
			dim as node_t ptr expr
			read_token( )
			expr = parse_while( )
			emit_line( "while(" & expr_to_str( expr ) & ") {" )
			indent += 1
		elseif lcase( tk_str ) = "if" then
			dim as node_t ptr expr
			read_token( )
			expr = parse_if( )
			emit_line( "if(" & expr_to_str( expr ) & ") {" )
			indent += 1
			symstack_push( )
		elseif lcase( tk_str ) = "elseif" then
			dim as node_t ptr expr
			read_token( )
			expr = parse_if( )
			indent -= 1
			symstack_pop( )
			emit_line( "} else if(" & expr_to_str( expr ) & ") {" )
			indent += 1
			symstack_push( )
		elseif lcase( tk_str ) = "else" then
			read_token( )
			if match( TK_EOL ) = 0 then
				expected( "end of line", tk_str )
			end if
			indent -= 1
			symstack_pop( )
			emit_line( "} else {" )
			indent += 1
			symstack_push( )
		elseif lcase( tk_str ) = "do" then
			read_token( )
			if match( TK_EOL ) = 0 then
				expected( "end of line", tk_str )
			end if
			emit_line( "do {" )
			indent += 1
		elseif lcase( tk_str ) = "loop" then
			read_token( )
			if match( TK_EOL ) = 0 then
				expected( "end of line", tk_str )
			end if
			indent -= 1
			emit_line( "} while(1);" )
		elseif lcase( tk_str ) = "wend" then
			read_token( )
			if match( TK_EOL ) = 0 then
				expected( "end of line", tk_str )
			end if
			indent -= 1
			emit_line( "}" )
		elseif lcase( tk_str ) = "end" then
			read_token( )
			if match_str( "sub" ) then
				emit_line( "return;" )
				symstack_pop( )
			elseif match_str( "function" ) then
				emit_line( "return func$result;" )
				symstack_pop( )
			elseif match_str( "if" ) then
				symstack_pop( )
			else
				read_token( )
			end if
			if match( TK_EOL ) = 0 then
				expected( "end of line", tk_str )
			end if
			indent -= 1
			emit_line( "}" )
		elseif lcase( tk_str ) = "exit" then
			read_token( )
			read_token( )
			if match( TK_EOL ) = 0 then
				expected( "end of line", tk_str )
			end if
			emit_line( "break;" )
		elseif tk_typ = TK_IDENT then
			dim as datatype_t ptr dt = callocate( sizeof( datatype_t ) )
			dim as string s = parse_lvalue( dt )
			dim as node_t ptr expr
			if tk_str = "=" then
				if match_str( "=" ) = 0 then
					expected( "'='", tk_str )
				end if
				expr = parse_expression( )
				if match( TK_EOL ) = 0 then
					expected( "end of line", tk_str )
				end if
				s += " = (" & dt->s & ")" & expr_to_str( expr )
			elseif tk_str = "+=" then
				if match_str( "+=" ) = 0 then
					expected( "'+='", tk_str )
				end if
				expr = parse_expression( )
				if match( TK_EOL ) = 0 then
					expected( "end of line", tk_str )
				end if
				s += " += " & expr_to_str( expr )
			elseif tk_str = "-=" then
				if match_str( "-=" ) = 0 then
					expected( "'-='", tk_str )
				end if
				expr = parse_expression( )
				if match( TK_EOL ) = 0 then
					expected( "end of line", tk_str )
				end if
				s += " -= " & expr_to_str( expr )
			elseif tk_str = ":" then
				if match_str( ":" ) = 0 then
					expected( "':'", tk_str )
				end if
				if match( TK_EOL ) = 0 then
					expected( "end of line", tk_str )
				end if
				s += ":"
				emit_line( s )
				goto moop
			else
				' function call
				if match( TK_EOL ) = 0 then
					expected( "end of line", tk_str )
				end if
			end if
			emit_line( s & ";" )
		elseif tk_typ = TK_EOL then
			read_token( )
		else
			print "Expected keyword, found '" & tk_str & "'"
			exit_( 1 )
		end if
	wend

end sub
