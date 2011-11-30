#include once "crt.bi"
#undef Rem
#define Rem

Dim Shared As Integer CHAR_DBLQUOTE = 34

Dim Shared As Integer SYM_BAD      = 0
Dim Shared As Integer SYM_DATATYPE = 1
Dim Shared As Integer SYM_VAR      = 2
Dim Shared As Integer SYM_PROC     = 3
Dim Shared As Integer SYM_ARRAY    = 4
Dim Shared As Integer SYM_LABEL    = 5

Type sym_t
        typ As Integer
        ident As String
        dt As Any Ptr
        is_array As Integer
        is_shared As Integer
        array_size As Integer
        dt_parent As sym_t Ptr
        members As sym_t Ptr
        member_count As Integer
End Type

Type datatype_t
        dt As sym_t Ptr
        ptr_cnt As Integer
        s As String
        sym As sym_t Ptr
        expr(0 To 15) As Any Ptr
        expr_cnt As Integer
        is_proc As Integer
End Type

Type symtab_t
        syms(0 To 255) As sym_t
        sym_count As Integer
End Type

Type symstack_t
        stk(0 To 15) As symtab_t
        stk_p As Integer
End Type

Dim Shared As Integer NODE_BAD      = 0
Dim Shared As Integer NODE_LITINT   = 1
Dim Shared As Integer NODE_LITSTR   = 2
Dim Shared As Integer NODE_BOP      = 3
Dim Shared As Integer NODE_LVALUE   = 4
Dim Shared As Integer NODE_UOP      = 5
Dim Shared As Integer NODE_CONVERT  = 6
Dim Shared As Integer NODE_PROCCALL = 7
Dim Shared As Integer NODE_DIM      = 8
Type node_t
        typ As Integer
        s As String
        l As node_t Ptr
        r As node_t Ptr
        dt As datatype_t
        expr(0 To 15) As node_t Ptr
        expr_cnt As Integer
        sym As sym_t Ptr

        dim_sym        As sym_t Ptr
        dim_dt         As datatype_t Ptr
        dim_init_expr  As node_t Ptr

End Type

Dim Shared As Integer TK_BAD     = 0
Dim Shared As Integer TK_IDENT   = 256
Dim Shared As Integer TK_LITINT  = 257
Dim Shared As Integer TK_EOF     = 258
Dim Shared As Integer TK_LITSTR  = 259
Dim Shared As Integer TK_EOL     = 260
Dim Shared As Integer TK_SELFADD = 261
Dim Shared As Integer TK_NE      = 262
Dim Shared As Integer TK_SELFSUB = 263
Dim Shared As Integer TK_ARROW   = 264
Dim Shared As Integer TK_GE      = 265

Dim Shared As Integer look
Dim Shared As String  tk_str
Dim Shared As Integer tk_typ

Dim Shared As symstack_t symstack

Dim Shared As Integer indent

Dim Shared As Integer curr_line

'*******************************************************************************
' Emit
'*******************************************************************************

'::::::::
Sub emit_line _
        ( _
                Byref txt As String _
        )

        Dim As Integer i

        i = 0
        While i < indent
                Print !"\t";
                i += 1
        Wend

        Print txt

End Sub

'*******************************************************************************
' Error
'*******************************************************************************

'::::::::
Sub expected _
        ( _
                Byref what  As String, _
                Byref found As String _
        )

        Print "Expected : " & what
        Print "Found    : " & found
        Print "Line     : " & curr_line
        exit_( 1 )

End Sub

'*******************************************************************************
' Symbol table
'*******************************************************************************

'::::::::
Sub symstack_push _
        ( _
        )

        symstack.stk_p += 1
        symstack.stk(symstack.stk_p).sym_count = 0

End Sub

'::::::::
Sub symstack_pop _
        ( _
        )

        symstack.stk_p -= 1

End Sub

'::::::::
Function sym_exists _
        ( _
                Byref ident As String _
        ) As Integer

        Dim As Integer i

        i = 0
        While i < symstack.stk(symstack.stk_p).sym_count
                If Lcase( symstack.stk(symstack.stk_p).syms(i).ident ) = Lcase( ident ) Then
                        Return -1
                End If
                i += 1
        Wend

End Function

'::::::::
Function sym_find _
        ( _
                Byref ident As String, _
                Byval sym As sym_t Ptr _
        ) As sym_t Ptr

        Dim As Integer i
        Dim As Integer p = symstack.stk_p

        'print __FUNCTION__

        If sym = 0 Then
                While p >= 0
                        i = 0
                        While i < symstack.stk(p).sym_count
                                'print lcase( symstack.stk(p).syms(i).ident ) & " " & lcase( ident )
                                If Lcase( symstack.stk(p).syms(i).ident ) = Lcase( ident ) Then
                                        Return @symstack.stk(p).syms(i)
                                End If
                                i += 1
                        Wend
                        p -= 1
                Wend
        Else
                Dim As datatype_t Ptr dt = sym->dt
                If dt Then
                        If dt->dt Then
                                i = 0
                                While i < dt->dt->member_count
                                        'print lcase( dt->dt->members[i].ident ) & " " & lcase( ident )
                                        If Lcase( dt->dt->members[i].ident ) = Lcase( ident ) Then
                                                Return @dt->dt->members[i]
                                        End If
                                        i += 1
                                Wend
                        End If
                End If
                If sym->dt_parent Then
                        Print "not yet done!"
                End If
        End If

End Function

'::::::::
Function sym_add_type _
        ( _
                Byref ident As String, _
                Byval member_count As Integer, _
                Byval members As sym_t Ptr _
        ) As sym_t Ptr

        Dim As Integer _pos = symstack.stk(0).sym_count

        symstack.stk(0).syms(_pos).typ = SYM_DATATYPE
        symstack.stk(0).syms(_pos).ident = ident
        symstack.stk(0).syms(_pos).member_count = member_count
        symstack.stk(0).syms(_pos).members = members

        Function = @symstack.stk(0).syms(_pos)

        symstack.stk(0).sym_count += 1

End Function

'::::::::
Function sym_add_proc _
        ( _
                Byref ident As String _
        ) As sym_t Ptr

        Dim As Integer _pos = symstack.stk(0).sym_count

        symstack.stk(0).syms(_pos).typ = SYM_PROC
        symstack.stk(0).syms(_pos).ident = ident
        symstack.stk(0).syms(_pos).dt = callocate( sizeof( datatype_t ) )

        Dim As datatype_t Ptr dt = symstack.stk(0).syms(_pos).dt
        dt->s = "<PROC>"

        Function = @symstack.stk(0).syms(_pos)

        symstack.stk(0).sym_count += 1

End Function

'::::::::
Function sym_add_dim _
        ( _
                Byref ident As String, _
                Byval dt As datatype_t Ptr, _
                Byval is_array As Integer, _
                Byval array_size As Integer, _
                Byval is_shared As Integer _
        ) As sym_t Ptr

        Dim As Integer _pos = symstack.stk(symstack.stk_p).sym_count
        Dim As sym_t Ptr dt_parent

        dt_parent = sym_find( dt->s, 0 )

        If is_array Then
                symstack.stk(symstack.stk_p).syms(_pos).typ = SYM_ARRAY
        Else
                symstack.stk(symstack.stk_p).syms(_pos).typ = SYM_VAR
        End If
        symstack.stk(symstack.stk_p).syms(_pos).ident = ident
        symstack.stk(symstack.stk_p).syms(_pos).dt = dt
        symstack.stk(symstack.stk_p).syms(_pos).is_array = is_array
        symstack.stk(symstack.stk_p).syms(_pos).array_size = array_size
        symstack.stk(symstack.stk_p).syms(_pos).is_shared = is_shared
        symstack.stk(symstack.stk_p).syms(_pos).dt_parent = dt_parent

        Function = @symstack.stk(symstack.stk_p).syms(_pos)

        symstack.stk(symstack.stk_p).sym_count += 1

End Function

'::::::::
Sub sym_add_label _
        ( _
                Byref ident As String _
        )

        Dim As Integer _pos = symstack.stk(symstack.stk_p).sym_count

        symstack.stk(symstack.stk_p).syms(_pos).typ = SYM_LABEL
        symstack.stk(symstack.stk_p).syms(_pos).ident = ident

        symstack.stk(symstack.stk_p).sym_count += 1

End Sub

'*******************************************************************************
' Lexer
'*******************************************************************************

'::::::::
Sub read_char _
        ( _
        )

        look = fgetc( stdin )

End Sub

'::::::::
Sub skip_white _
        ( _
        )

        While (look = Asc( " " )) Or (look = Asc( !"\t" ))
                read_char( )
        Wend

End Sub

'::::::::
Function read_ident _
        ( _
        ) As String

        Dim As String result

        While isalnum( look ) Or (look = Asc( "_" ))
                result += Chr( look )
                read_char( )
        Wend

        Function = result

End Function

'::::::::
Function read_litint _
        ( _
        ) As String

        Dim As String result

        While isdigit( look )
                result += Chr( look )
                read_char( )
        Wend

        Function = result

End Function

'::::::::
Function read_litstr _
        ( _
        ) As String

        Dim As String result

        If look <> CHAR_DBLQUOTE Then
                Print !"Expected '" & Chr( CHAR_DBLQUOTE ) & "'"
                exit_( 1 )
        End If

        read_char( )

        While (look <> EOF_) And (look <> CHAR_DBLQUOTE)
                result += Chr( look )
                read_char( )
        Wend

        If look <> CHAR_DBLQUOTE Then
                Print !"Expected '" & Chr( CHAR_DBLQUOTE ) & "'"
                exit_( 1 )
        End If

        read_char( )

        Function = "str_temp(" & Chr( CHAR_DBLQUOTE )& result & Chr( CHAR_DBLQUOTE ) & ")"

End Function

'::::::::
Sub read_token _
        ( _
        )

read_token_start:

        skip_white( )

        tk_str = "<<BAD>>"
        tk_typ = TK_BAD

        If look = EOF_ Then
                tk_str = "<<EOF>>"
                tk_typ = TK_EOF
        Elseif isalpha( look ) Or (look = Asc( "_" )) Then
                tk_str = read_ident( )
                tk_typ = TK_IDENT
                If tk_str = "_" Then
                        While (look = 10) Or (look = 13)
                                read_char( )
                        Wend
                        Goto read_token_start
                End If
        Elseif isdigit( look ) Then
                tk_str = read_litint( )
                tk_typ = TK_LITINT
        Elseif look = CHAR_DBLQUOTE Then
                tk_str = read_litstr( )
                tk_typ = TK_LITSTR
        Elseif look = Asc( "!" ) Then
                read_char( )
                tk_str = read_litstr( )
                tk_typ = TK_LITSTR
        Elseif (look = 10) Or (look = 13) Then
                While (look = 10) Or (look = 13) Or (look = Asc( " " )) Or (look = Asc( !"\t" ))
                        read_char( )
                Wend
                tk_str = "<<EOL>>"
                tk_typ = TK_EOL
        Elseif (look = Asc( "'" )) Then
                While (look <> 10) And (look <> 13) And (look <> EOF_)
                        read_char( )
                Wend
                'while (look = 10) or (look = 13)
                '        read_char( )
                'wend
                Goto read_token_start
        Elseif look = Asc( "+" ) Then
                tk_str = Chr( look )
                tk_typ = look
                read_char( )
                If look = Asc( "=" ) Then
                        tk_str = "+="
                        tk_typ = TK_SELFADD
                        read_char( )
                End If
        Elseif look = Asc( "-" ) Then
                tk_str = Chr( look )
                tk_typ = look
                read_char( )
                If look = Asc( "=" ) Then
                        tk_str = "-="
                        tk_typ = TK_SELFSUB
                        read_char( )
                Elseif look = Asc( ">" ) Then
                        tk_str = "->"
                        tk_typ = TK_ARROW
                        read_char( )
                End If
        Elseif look = Asc( "<" ) Then
                tk_str = Chr( look )
                tk_typ = look
                read_char( )
                If look = Asc( ">" ) Then
                        tk_str = "<>"
                        tk_typ = TK_NE
                        read_char( )
                End If
        Elseif look = Asc( ">" ) Then
                tk_str = Chr( look )
                tk_typ = look
                read_char( )
                If look = Asc( "=" ) Then
                        tk_str = ">="
                        tk_typ = TK_GE
                        read_char( )
                End If
        Else
                tk_str = Chr( look )
                tk_typ = look
                read_char( )
        End If

        'print " // " & tk_str

        ''''print "<<" & tk_str & ">>"

End Sub

'::::::::
Function match _
        ( _
                Byval t As Integer _
        ) As Integer

        If tk_typ = t Then
                Function = -1
                read_token( )
        Else
                Function = 0
        End If

End Function

'::::::::
Function match_str _
        ( _
                Byref s As String _
        ) As Integer

        If Lcase( tk_str ) = Lcase( s ) Then
                Function = -1
                read_token( )
        Else
                Function = 0
        End If

End Function


'*******************************************************************************
' Tree
'*******************************************************************************

'::::::::
Function tree_node_litint _
        ( _
                Byref s As String _
        ) As node_t Ptr

        Dim As node_t Ptr node = callocate( sizeof( node_t ) )

        node->typ = NODE_LITINT
        node->s   = s
        node->dt.s = "integer"

        Function = node

End Function

'::::::::
Function tree_node_litstr _
        ( _
                Byref s As String _
        ) As node_t Ptr

        Dim As node_t Ptr node = callocate( sizeof( node_t ) )

        node->typ = NODE_LITSTR
        node->s   = s
        node->dt.s = "FBSTRING"

        Function = node

End Function

'::::::::
Function tree_node_bop _
        ( _
                Byref bop As String, _
                Byval l As node_t Ptr, _
                Byval r As node_t Ptr _
        ) As node_t Ptr

        Dim As node_t Ptr node = callocate( sizeof( node_t ) )

        node->typ = NODE_BOP
        node->s   = bop
        node->l   = l
        node->r   = r

        If l->dt.s <> r->dt.s Then
                If (l->dt.ptr_cnt > 0) And (r->dt.s = "integer") Then

                Else
                        'print "filae"
                        'print l->dt.s & " " & r->dt.s
                        'end 1
                End If
        End If

        node->dt = l->dt

        Function = node

End Function

'::::::::
Function tree_node_uop _
        ( _
                Byref uop As String, _
                Byval l As node_t Ptr _
        ) As node_t Ptr

        Dim As node_t Ptr node = callocate( sizeof( node_t ) )

        node->typ = NODE_UOP
        node->s   = uop
        node->l   = l

        node->dt = l->dt

        Function = node

End Function

'::::::::
Function tree_node_convert _
        ( _
                Byval l As node_t Ptr, _
                Byval dt As String _
        ) As node_t Ptr

        Dim As node_t Ptr node = callocate( sizeof( node_t ) )

        node->typ = NODE_CONVERT
        node->l   = l
        node->dt.s = dt

        Function = node

End Function

'::::::::
Function tree_node_proccall _
        ( _
                Byval sym As sym_t Ptr, _
                Byval expr As node_t Ptr ptr, _
                Byval expr_cnt As Integer _
        ) As node_t Ptr

        Dim As node_t Ptr node = callocate( sizeof( node_t ) )

        node->typ = NODE_PROCCALL
        node->sym = sym
        node->expr_cnt = expr_cnt

        Dim As Integer i = 0
        While i < expr_cnt
                node->expr(i) = expr[i]
                i += 1
        Wend

        Function = node

End Function

'::::::::
Function tree_node_lvalue _
        ( _
                Byref s As String, _
                Byval dt As datatype_t Ptr _
        ) As node_t Ptr

        If dt->expr_cnt > 0 Then
                If dt->is_proc Then
                        Dim As node_t Ptr ptr expr = @dt->expr(0)
                        Dim As node_t Ptr node = tree_node_proccall( dt->sym, expr, dt->expr_cnt )
                        Return node
                End If
        End If

        Dim As node_t Ptr node = callocate( sizeof( node_t ) )

        node->typ = NODE_LVALUE
        node->s   = s
        node->dt.s = dt->s
        node->dt.ptr_cnt = dt->ptr_cnt

        Function = node

End Function

'*******************************************************************************
' ???
'*******************************************************************************

'::::::::
Function expr_to_str _
        ( _
                Byval node As node_t Ptr _
        ) As String

        If node->typ = NODE_BOP Then
                Dim As String bop
                Dim As String l
                Dim As String r
                bop = node->s
                l = expr_to_str( node->l )
                r = expr_to_str( node->r )
                If bop = "=" Then
                        bop = "=="
                Elseif bop = "<>" Then
                        bop = "!="
                Elseif lcase( bop ) = "and" Then
                        bop = "&"
                Elseif lcase( bop ) = "or" Then
                        bop = "|"
                Elseif bop = "&" Then
                        bop = "concat"
                End If
                If bop = "concat" Then
                        Function = "concat(" & l & ", " & r & ")"
                Else
                        Function = "(" & l & " " & bop & " " & r & ")"
                End If
        Elseif node->typ = NODE_UOP Then
                Dim As String uop
                Dim As String l
                uop = node->s
                l = expr_to_str( node->l )
                If uop = "@" Then
                        uop = "&"
                End If
                Function = "(" & uop & " " & l & ")"
        Elseif node->typ = NODE_CONVERT Then
                Dim As String l
                l = expr_to_str( node->l )
                If (node->dt.s) = "FBSTRING" And (node->l->dt.s = "integer") Then
                        Function = "int_to_str(" & l & ")"
                Else
                        Print "No convert known!"
                        exit_( 1 )
                End If
        Elseif node->typ = NODE_PROCCALL Then
                Dim As String s
                s = node->sym->ident & "("
                Dim As Integer i = 0
                While i < node->expr_cnt
                        s += expr_to_str( node->expr(i) )
                        i += 1
                        If i < node->expr_cnt Then
                                s += ", "
                        End If
                Wend
                s += ")"
                Function = s
        Else
                Function = node->s
        End If

End Function

'::::::::
Function get_dt_size _
        ( _
                Byval dt As datatype_t Ptr _
        ) As Integer

        If dt->ptr_cnt > 0 Then
                Function = 4
        Elseif dt->s = "integer" Then
                Function = 4
        Else
                Function = 0
        End If

End Function

'*******************************************************************************
' Parser
'*******************************************************************************

'::::::::
Declare Function parse_expression _
        ( _
        ) As node_t Ptr

'::::::::
Sub parse_pp_include _
        ( _
        )

        match_str( "once" )

        'emit_line( "#include " & tk_str )

        If match( TK_LITSTR ) = 0 Then
                expected( "literal string", tk_str )
        End If

        If match( TK_EOL ) = 0 Then
                expected( "end of line", tk_str )
        End If

End Sub

'::::::::
Sub parse_pp_dummy _
        ( _
        )

        While (tk_typ <> TK_EOL) And (tk_typ <> TK_EOF)
                read_token( )
        Wend

        If match( TK_EOL ) = 0 Then
                expected( "end of line", tk_str )
        End If

End Sub

'::::::::
Function parse_lvalue _
        ( _
                Byval dt As datatype_t Ptr _
        ) As String

        Dim As String s
        Dim As node_t Ptr expr
        Dim As sym_t Ptr sym
        Dim As sym_t Ptr oldsym
        'dim as node_t ptr proc_expr(0 to 15)
        'dim as integer proc_expr_count
        'dim as integer is_proc

        Do
                'proc_expr_count = 0
                sym = sym_find( tk_str, oldsym )
                dt->sym = sym
                s += tk_str
                read_token( )
                If tk_str = ":" Then
                        sym_add_label( s )
                        Exit Do
                End If
                If sym = 0 Then
                        Print "!! " & s
                        expected( "symbol/member", tk_str )
                End If
                If tk_str = "(" Then
                        If sym->typ = SYM_ARRAY Then
                                s += "["
                        Else
                                s += "("
                                dt->is_proc = -1
                        End If
                        read_token( )
                        While (tk_str <> ")") And (tk_typ <> TK_EOF)
                                expr = parse_expression( )
                                s += expr_to_str( expr )
                                dt->expr(dt->expr_cnt) = expr
                                dt->expr_cnt += 1
                                If tk_str = "," Then
                                        s += ", "
                                        read_token( )
                                Else
                                        Exit While
                                End If
                        Wend
                        If match_str( ")" ) = 0 Then
                                expected( "')'", tk_str )
                        End If
                        If sym->typ = SYM_ARRAY Then
                                s += "]"
                        Else
                                s += ")"
                        End If
                Elseif tk_str = "[" Then
                        s += "["
                        read_token( )
                        expr = parse_expression( )
                        s += expr_to_str( expr )
                        If match_str( "]" ) = 0 Then
                                expected( "']'", tk_str )
                        End If
                        s += "]"
                End If
                If tk_str = "." Then
                        s += "."
                        read_token( )
                Elseif tk_str = "->" Then
                        s += "->"
                        read_token( )
                Else
                        Exit Do
                End If
                oldsym = sym
        Loop

        If sym Then
                Dim As datatype_t Ptr _dt = sym->dt
                If sym->typ <> SYM_DATATYPE Then
                        If _dt Then
                                dt->s = _dt->s
                                dt->ptr_cnt = _dt->ptr_cnt
                        End If
                End If
        End If

        Function = s

End Function

'::::::::
Function parse_atom _
        ( _
        ) As node_t Ptr

        If tk_typ = TK_LITINT Then
                Function = tree_node_litint( tk_str )
                read_token( )
        Elseif tk_typ = TK_LITSTR Then
                Function = tree_node_litstr( tk_str )
                read_token( )
        Elseif tk_typ = TK_IDENT Then
                Dim As datatype_t Ptr _dt = callocate( sizeof( datatype_t ) )
                Dim As String lvalue = parse_lvalue( _dt )
                Function = tree_node_lvalue( lvalue, _dt )
        Elseif tk_str = "(" Then
                read_token( )
                Function = parse_expression( )
                If match_str( ")" ) = 0 Then
                        expected( "')'", tk_str )
                End If
        Elseif tk_str = "-" Then
                read_token( )
                Function = tree_node_uop( "-", parse_expression( ) )
        Elseif tk_str = "@" Then
                read_token( )
                Function = tree_node_uop( "@", parse_expression( ) )
        Else
                expected( "atom", tk_str )
        End If

End Function

'::::::::
Function parse_muldiv _
        ( _
        ) As node_t Ptr

        Dim As node_t Ptr node

        node = parse_atom( )
        While (tk_str = "*") Or (tk_str = "/")
                Dim As String bop = tk_str
                read_token( )
                node = tree_node_bop( bop, node, parse_atom( ) )
        Wend

        Function = node

End Function

'::::::::
Function parse_addsub _
        ( _
        ) As node_t Ptr

        Dim As node_t Ptr node

        node = parse_muldiv( )
        While (tk_str = "+") Or (tk_str = "-")
                Dim As String bop = tk_str
                read_token( )
                node = tree_node_bop( bop, node, parse_muldiv( ) )
        Wend

        Function = node

End Function

'::::::::
Function parse_strconcat _
        ( _
        ) As node_t Ptr

        Dim As node_t Ptr node

        node = parse_addsub( )
        While (tk_str = "&")
                Dim As String bop = tk_str
                read_token( )
                node = tree_node_bop( bop, node, parse_addsub( ) )
                If node->l->dt.s <> node->r->dt.s Then
                        If (node->l->dt.s = "FBSTRING") And (node->r->dt.s = "integer") Then
                                node->r = tree_node_convert( node->r, "FBSTRING" )
                        Elseif (node->r->dt.s = "FBSTRING") And (node->l->dt.s = "integer") Then
                                node->l = tree_node_convert( node->l, "FBSTRING" )
                        End If
                End If
        Wend

        Function = node

End Function

'::::::::
Function parse_rel _
        ( _
        ) As node_t Ptr

        Dim As node_t Ptr node

        node = parse_strconcat( )
        While (tk_str = "=") Or (tk_str = "<>") _
           Or (tk_str = "<") Or (tk_str = "<=") _
           Or (tk_str = ">") Or (tk_str = ">=")
                Dim As String bop = tk_str
                read_token( )
                node = tree_node_bop( bop, node, parse_strconcat( ) )
        Wend

        Function = node

End Function

'::::::::
Function parse_and _
        ( _
        ) As node_t Ptr

        Dim As node_t Ptr node

        node = parse_rel( )
        While (lcase( tk_str ) = "and")
                Dim As String bop = tk_str
                read_token( )
                node = tree_node_bop( bop, node, parse_rel( ) )
        Wend

        Function = node

End Function

'::::::::
Function parse_or _
        ( _
        ) As node_t Ptr

        Dim As node_t Ptr node

        node = parse_and( )
        While (lcase( tk_str ) = "or")
                Dim As String bop = tk_str
                read_token( )
                node = tree_node_bop( bop, node, parse_and( ) )
        Wend

        Function = node

End Function

'::::::::
Function parse_expression _
        ( _
        ) As node_t Ptr

        Function = parse_or( )

End Function

'::::::::
Function parse_datatype _
        ( _
        ) As datatype_t Ptr

        Dim As datatype_t Ptr datatype = callocate( sizeof( datatype_t ) )
        Dim As String dt = tk_str
        Dim As Integer ptr_cnt
        Dim As Integer i
        Dim As String result

        If Lcase( dt ) = "string" Then
                dt = "FBSTRING"
        Elseif Lcase( dt ) = "any" Then
                dt = "void"
        Elseif Lcase( dt ) = "integer" Then
                dt = "integer"
        Elseif Lcase( dt ) = "zstring" Then
                dt = "zstring"
        End If

        read_token( )

        While Lcase( tk_str ) = "ptr"
                ptr_cnt += 1
                read_token( )
        Wend

        result = dt

        i = 0
        While i < ptr_cnt
                result += " *"
                i += 1
        Wend       

        datatype->dt = sym_find( dt, 0 )
        datatype->ptr_cnt = ptr_cnt
        datatype->s = result

        Function = datatype
       
End Function

'::::::::
Function dim_to_str _
        ( _
                Byval node As node_t Ptr _
        ) As String

        Dim As String s
        Dim As Integer do_memset

        s = node->dim_dt->s & " " & node->dim_sym->ident

        If node->dim_sym->is_array Then
                s += "[" & node->dim_sym->array_size & "]"
        End If

        If node->dim_init_expr = 0 Then
                If node->dim_dt->s = "integer" Then
                        If node->dim_sym->is_array = 0 Then
                                node->dim_init_expr = tree_node_litint( "0" )
                        Else
                                do_memset = -1
                        End If
                Elseif node->dim_dt->ptr_cnt > 0 Then
                        If node->dim_sym->is_array = 0 Then
                                node->dim_init_expr = tree_node_litint( "0" )
                        Else
                                do_memset = -1
                        End If
                End If
        End If

        If node->dim_init_expr Then
                s += " = (" & node->dim_dt->s & ")" & expr_to_str( node->dim_init_expr )
        End If

        s += ";"

        If do_memset Then
                Dim As Integer sz = get_dt_size( node->dim_dt ) * node->dim_sym->array_size
                If sz <> 0 Then
                        s += !"\nmemset(" & node->dim_sym->ident & ", 0, " & sz & ");"
                Else
                        s += !"\n/* memset(" & node->dim_sym->ident & ", 0, " & sz & "); */"
                End If
        End If

        Function = s

End Function

'::::::::
Function parse_dim _
        ( _
        ) As node_t Ptr

        Dim As datatype_t Ptr dt
        Dim As String ident
        Dim As node_t Ptr expr
        Dim As Integer is_array
        Dim As Integer array_size
        Dim As Integer is_shared
        Dim As sym_t Ptr sym

        If match_str( "shared" ) Then
                is_shared = -1
        End If

        If match_str( "as" ) = 0 Then
                expected( "AS", tk_str )
        End If

        dt = parse_datatype( )

        ident = tk_str
        read_token( ) ' dump ident

        ' Will it be an array?
        If match_str( "(" ) Then
                is_array = -1
                If match_str( "0" ) = 0 Then
                        expected( "0", tk_str )
                End If
                If match_str( "to" ) = 0 Then
                        expected( "TO", tk_str )
                End If
                array_size = Val(tk_str) + 1
                read_token( ) ' dump ubound
                If match_str( ")" ) = 0 Then
                        expected( "')'", tk_str )
                End If
        End If

        ' Is there an initializer?
        If match_str( "=" ) Then
                expr = parse_expression( )
        End If

        If match( TK_EOL ) = 0 Then
                expected( "end of line", tk_str )
        End If

        ' No symbol with this name?
        If sym_exists( ident ) Then
                Print "Symbol '" & ident & "' already exists!"
                exit_( 1 )
        End If

        ' Allocate the symbol
        sym = sym_add_dim( ident, dt, is_array, array_size, is_shared )

        Dim As node_t Ptr node = callocate( sizeof( node_t ) )

        node->typ = NODE_DIM
        node->dim_sym = sym
        node->dim_dt = dt
        node->dim_init_expr = expr

        Function = node

End Function

'::::::::
Sub parse_sub _
        ( _
                Byval is_decl As Integer _
        )

        Dim As String ident
        Dim As datatype_t Ptr dt
        Dim As String param_ident(0 To 255)
        Dim As datatype_t Ptr param_dt(0 To 255)
        Dim As Integer param_is_byval(0 To 255)
        Dim As Integer param_is_byref(0 To 255)
        Dim As Integer p_count
        Dim As String s
        Dim As Integer i

        ident = tk_str
        read_token( )

        If match_str( "(" ) = 0 Then
                expected( "'('", tk_str )
        End If

        symstack_push( )

        While (tk_str <> ")") And (tk_typ <> TK_EOF)
                If match_str( "byref" ) Then
                        param_is_byval(p_count) = 0
                        param_is_byref(p_count) = -1
                Elseif match_str( "byval" ) Then
                        param_is_byval(p_count) = -1
                        param_is_byref(p_count) = 0
                Else
                        expected( "byval/byref", tk_str )
                End If
                param_ident(p_count) = tk_str
                read_token( )
                If match_str( "as" ) = 0 Then
                        expected( "as", tk_str )
                End If
                param_dt(p_count) = parse_datatype( )
                sym_add_dim( param_ident(p_count), param_dt(p_count), 0, 0, 0 )
                p_count += 1
                If tk_str = "," Then
                        read_token( )
                Else
                        Exit While
                End If
        Wend

        If match_str( ")" ) = 0 Then
                expected( "')'", tk_str )
        End If

        If match_str( "as" ) Then
                dt = parse_datatype( )
        Else
                dt = 0
        End If

        If match( TK_EOL ) = 0 Then
                expected( "end of line", tk_str )
        End If

        emit_line( "/*::::::::*/" )
        If dt Then
                s = dt->s & " "
        Else
                s = "void "
        End If

        s += ident & "("

        i = 0
        While i < p_count
                If param_is_byref(i) Then
                        's += "const "
                End If
                s += param_dt(i)->s
                If param_is_byref(i) Then
                        s += "&"
                End If
                s += " " & param_ident(i)
                i += 1
                If i <> p_count Then
                        s += ", "
                End If
        Wend
       
        s += ")"

        If is_decl Then
                emit_line( s & ";" )
        Else
                emit_line( s )
                emit_line( "{" )
                indent += 1
                If dt Then
                        emit_line( dt->s & " func$result = 0;" )
                End If
        End If

        sym_add_proc( ident )

End Sub

'::::::::
Function parse_while _
        ( _
        ) As node_t Ptr

        Function = parse_expression( )

        If match( TK_EOL ) = 0 Then
                expected( "end of line", tk_str )
        End If

End Function

'::::::::
Function parse_if _
        ( _
        ) As node_t Ptr

        Function = parse_expression( )

        If match_str( "then" ) = 0 Then
                expected( "THEN", tk_str )
        End If

        If match( TK_EOL ) = 0 Then
                expected( "end of line", tk_str )
        End If

End Function

'::::::::
Sub parse_type _
        ( _
        )

        Dim As String  ident
        Dim As Integer member_count
        Dim As sym_t Ptr members = callocate( sizeof( sym_t ) * 256 )
        Dim As Integer i

        ident = tk_str
        read_token( ) ' dump identifier

        If match( TK_EOL ) = 0 Then
                expected( "end of line", tk_str )
        End If

        While (Lcase( tk_str ) <> "end") And (tk_typ <> TK_EOF)
                members[member_count].ident = tk_str
                read_token( ) ' dump identifier
                If match_str( "(" ) Then
                        members[member_count].typ = SYM_ARRAY
                        If match_str( "0" ) = 0 Then
                                expected( "0", tk_str )
                        End If
                        If match_str( "to" ) = 0 Then
                                expected( "TO", tk_str )
                        End If
                        members[member_count].array_size = Val(tk_str) + 1
                        read_token( )
                        If match_str( ")" ) = 0 Then
                                expected( "')'", tk_str )
                        End If
                Else
                        members[member_count].typ = SYM_VAR
                End If
                If match_str( "as" ) = 0 Then
                        expected( "AS", tk_str )
                End If
                members[member_count].dt = parse_datatype( )
                If match( TK_EOL ) = 0 Then
                        expected( "end of line", tk_str )
                End If
                member_count += 1
        Wend

        If match_str( "end" ) = 0 Then
                expected( "END", tk_str )
        End If

        If match_str( "type" ) = 0 Then
                expected( "TYPE", tk_str )
        End If

        If match( TK_EOL ) = 0 Then
                expected( "end of line", tk_str )
        End If

        If sym_exists( ident ) Then
                Print "Symbol '" & ident & "' already exists!"
                exit_( 1 )
        End If

        Dim As sym_t Ptr sym

        sym = sym_add_type( ident, member_count, members )

        emit_line( "/*::::::::*/" )
        emit_line( "struct " & ident & " {" )
        indent += 1
        i = 0
        While i < member_count
                Dim As String s
                Dim As datatype_t Ptr dt = members[i].dt
                If dt->dt = 0 Then
                        dt->dt = sym
                        Dim As datatype_t Ptr tmp = sym->members[i].dt
                        tmp->dt->dt = sym
                End If
                s = dt->s & " " & members[i].ident
                If members[i].typ = SYM_ARRAY Then
                        s += "[" & members[i].array_size & "]"
                End If
                emit_line( s & ";" )
                i += 1
        Wend
        indent -= 1
        emit_line( "};" )
        emit_line( "" )

End Sub

'::::::::
Sub parse_file _
        ( _
        )

        moop:
        While tk_typ <> TK_EOF
                If tk_str = "#" Then
                        read_token( )
                        If Lcase( tk_str ) = "include" Then
                                read_token( )
                                parse_pp_include( )
                        Elseif Lcase( tk_str ) = "undef" Then
                                parse_pp_dummy( )
                        Elseif Lcase( tk_str ) = "define" Then
                                parse_pp_dummy( )
                        Else
                                Print "Expected preprocessor command"
                                exit_( 1 )
                        End If
                Elseif Lcase( tk_str ) = "rem" Then
                        parse_pp_dummy( )
                Elseif match_str( "dim" ) Then
                        Dim As node_t Ptr node
                        Dim As String s
                        node = parse_dim( )
                        s = dim_to_str( node )
                        emit_line( s )
                Elseif Lcase( tk_str ) = "type" Then
                        read_token( )
                        parse_type( )
                Elseif (Lcase( tk_str ) = "declare") Then
                        read_token( )
                        read_token( )
                        parse_sub( -1 )
                Elseif (Lcase( tk_str ) = "sub") Or Lcase( tk_str ) = "function" Then
                        Dim As Integer is_func = Lcase( tk_str ) = "function"
                        Dim As node_t Ptr expr
                        read_token( )
                        If is_func Then
                                If tk_str = "=" Then
                                        If match_str( "=" ) = 0 Then
                                                expected( "'='", tk_str )
                                        End If
                                        expr = parse_expression( )
                                        If match( TK_EOL ) = 0 Then
                                                expected( "end of line", tk_str )
                                        End If
                                        emit_line( "func$result = " & expr_to_str( expr ) & ";" )
                                Else
                                        parse_sub( 0 )
                                End If
                        Else
                                parse_sub( 0 )
                        End If
                Elseif Lcase( tk_str ) = "print" Then
                        Dim As node_t Ptr expr
                        Dim As Integer has_semi
                        read_token( )
                        expr = parse_expression( )
                        has_semi = match_str( ";" )
                        If match( TK_EOL ) = 0 Then
                                expected( "end of line", tk_str )
                        End If
                        If has_semi Then
                                emit_line( "print(" & expr_to_str( expr ) & ", 0);" )
                        Else
                                emit_line( "print(" & expr_to_str( expr ) & ", 1);" )
                        End If
                Elseif Lcase( tk_str ) = "return" Then
                        Dim As node_t Ptr expr
                        read_token( )
                        expr = parse_expression( )
                        If match( TK_EOL ) = 0 Then
                                expected( "end of line", tk_str )
                        End If
                        emit_line( "return(" & expr_to_str( expr ) & ");" )
                Elseif Lcase( tk_str ) = "goto" Then
                        'dim as node_t ptr expr
                        Dim As String s
                        read_token( )
                        s = tk_str
                        read_token( )
                        'expr = parse_expression( )
                        If match( TK_EOL ) = 0 Then
                                expected( "end of line", tk_str )
                        End If
                        emit_line( "goto " & s & ";" )
                Elseif Lcase( tk_str ) = "while" Then
                        Dim As node_t Ptr expr
                        read_token( )
                        expr = parse_while( )
                        emit_line( "while(" & expr_to_str( expr ) & ") {" )
                        indent += 1
                Elseif Lcase( tk_str ) = "if" Then
                        Dim As node_t Ptr expr
                        read_token( )
                        expr = parse_if( )
                        emit_line( "if(" & expr_to_str( expr ) & ") {" )
                        indent += 1
                        symstack_push( )
                Elseif Lcase( tk_str ) = "elseif" Then
                        Dim As node_t Ptr expr
                        read_token( )
                        expr = parse_if( )
                        indent -= 1
                        symstack_pop( )
                        emit_line( "} else if(" & expr_to_str( expr ) & ") {" )
                        indent += 1
                        symstack_push( )
                Elseif Lcase( tk_str ) = "else" Then
                        read_token( )
                        If match( TK_EOL ) = 0 Then
                                expected( "end of line", tk_str )
                        End If
                        indent -= 1
                        symstack_pop( )
                        emit_line( "} else {" )
                        indent += 1
                        symstack_push( )
                Elseif Lcase( tk_str ) = "do" Then
                        read_token( )
                        If match( TK_EOL ) = 0 Then
                                expected( "end of line", tk_str )
                        End If
                        emit_line( "do {" )
                        indent += 1
                Elseif Lcase( tk_str ) = "loop" Then
                        read_token( )
                        If match( TK_EOL ) = 0 Then
                                expected( "end of line", tk_str )
                        End If
                        indent -= 1
                        emit_line( "} while(1);" )
                Elseif Lcase( tk_str ) = "wend" Then
                        read_token( )
                        If match( TK_EOL ) = 0 Then
                                expected( "end of line", tk_str )
                        End If
                        indent -= 1
                        emit_line( "}" )
                Elseif Lcase( tk_str ) = "end" Then
                        read_token( )
                        If match_str( "sub" ) Then
                                emit_line( "return;" )
                                symstack_pop( )
                        Elseif match_str( "function" ) Then
                                emit_line( "return func$result;" )
                                symstack_pop( )
                        Elseif match_str( "if" ) Then
                                symstack_pop( )
                        Else
                                read_token( )
                        End If
                        If match( TK_EOL ) = 0 Then
                                expected( "end of line", tk_str )
                        End If
                        indent -= 1
                        emit_line( "}" )
                Elseif Lcase( tk_str ) = "exit" Then
                        read_token( )
                        read_token( )
                        If match( TK_EOL ) = 0 Then
                                expected( "end of line", tk_str )
                        End If
                        emit_line( "break;" )
                Elseif tk_typ = TK_IDENT Then
                        Dim As datatype_t Ptr dt = callocate( sizeof( datatype_t ) )
                        Dim As String s = parse_lvalue( dt )
                        Dim As node_t Ptr expr
                        If tk_str = "=" Then
                                If match_str( "=" ) = 0 Then
                                        expected( "'='", tk_str )
                                End If
                                expr = parse_expression( )
                                If match( TK_EOL ) = 0 Then
                                        expected( "end of line", tk_str )
                                End If
                                s += " = (" & dt->s & ")" & expr_to_str( expr )
                        Elseif tk_str = "+=" Then
                                If match_str( "+=" ) = 0 Then
                                        expected( "'+='", tk_str )
                                End If
                                expr = parse_expression( )
                                If match( TK_EOL ) = 0 Then
                                        expected( "end of line", tk_str )
                                End If
                                s += " += " & expr_to_str( expr )
                        Elseif tk_str = "-=" Then
                                If match_str( "-=" ) = 0 Then
                                        expected( "'-='", tk_str )
                                End If
                                expr = parse_expression( )
                                If match( TK_EOL ) = 0 Then
                                        expected( "end of line", tk_str )
                                End If
                                s += " -= " & expr_to_str( expr )
                        Elseif tk_str = ":" Then
                                If match_str( ":" ) = 0 Then
                                        expected( "':'", tk_str )
                                End If
                                If match( TK_EOL ) = 0 Then
                                        expected( "end of line", tk_str )
                                End If
                                s += ":"
                                emit_line( s )
                                Goto moop
                        Else
                                ' function call
                                If match( TK_EOL ) = 0 Then
                                        expected( "end of line", tk_str )
                                End If
                        End If
                        emit_line( s & ";" )
                Elseif tk_typ = TK_EOL Then
                        read_token( )
                Else
                        Print "Expected keyword, found '" & tk_str & "'"
                        exit_( 1 )
                End If
        Wend

End Sub

'*******************************************************************************
' Main
'*******************************************************************************

'::::::::
Function main _
        ( _
                Byval argc As Integer, _
                Byval argv As Zstring Ptr ptr _
        ) As Integer

        Print "#include <stdlib.h>"
        Print "#include <stdio.h>"
        Print "#include <string.h>"
        Print "#include <ctype.h>"
        Print "struct FBSTRING {"
        Print !"\tFBSTRING();"
        Print !"\tFBSTRING(const char *);"
        Print !"\tFBSTRING(const FBSTRING&);"
        Print !"\t~FBSTRING();"
        Print !"\tint operator=(const char*);"
        Print !"\tint operator=(const FBSTRING&);"
        Print !"\tint operator==(const FBSTRING&);"
        Print !"\tint operator!=(const FBSTRING&);"
        Print !"\tint operator+=(const FBSTRING&);"
        'print !"\toperator const FBSTRING&();"
        'print !"\toperator const FBSTRING();"
        'print !"\toperator FBSTRING&();"
        'print !"\toperator FBSTRING();"
        Print !"\tvoid *data;"
        Print !"\tint len;"
        Print !"\tint size;"
        Print "};"
        Print "typedef int integer;"
        Print "typedef char zstring;"
        Print "void print(const FBSTRING&, integer nl);"
        Print "FBSTRING& concat(const FBSTRING&, const FBSTRING&);"
        Print "FBSTRING& lcase(const FBSTRING&);"
        Print "FBSTRING& int_to_str(integer);"
        Print "FBSTRING& chr(integer);"
        Print "integer asc(const FBSTRING&);"
        Print "integer val(const FBSTRING&);"
        Print "#define exit_ exit"
        Print "#define EOF_ EOF"
        Print "#define callocate(n) calloc(n, 1 )"
       
        ' CONSTRUCTORS

        Print "FBSTRING::FBSTRING()"
        Print "{"
        Print !"\tdata = 0;"
        Print !"\tlen = 0;"
        Print !"\tsize = 0;"
        Print "}"

        Print "FBSTRING::FBSTRING(const char *s)"
        Print "{"
        Print !"\tdata = 0;"
        Print !"\tlen = 0;"
        Print !"\tsize = 0;"
        Print !"\tif(s == 0) return;"
        Print !"\tint s_len = strlen(s);"
        Print !"\tdata = malloc(s_len);"
        Print !"\tlen = s_len;"
        Print !"\tsize = s_len;"
        Print !"\tmemcpy(data, s, s_len);"
        Print "}"

        Print "FBSTRING::FBSTRING(const FBSTRING& s)"
        Print "{"
        Print !"\tdata = 0;"
        Print !"\tlen = 0;"
        Print !"\tsize = 0;"
        Print !"\tif(s.data == 0) return;"
        Print !"\tdata = malloc(s.len);"
        Print !"\tlen = s.len;"
        Print !"\tsize = s.len;"
        Print !"\tmemcpy(data, s.data, s.len);"
        Print "}"

        ' DTOR

        Print "FBSTRING::~FBSTRING()"
        Print "{"
        'print !"\tif(data) free(data);"
        Print !"\tdata = 0;"
        Print !"\tlen = 0;"
        Print !"\tsize = 0;"
        Print "}"

        ' LET

        Print "int FBSTRING::operator=(const char *s)"
        Print "{"
        Print !"\tif(data) free(data);"
        Print !"\tdata = 0;"
        Print !"\tlen = 0;"
        Print !"\tsize = 0;"
        Print !"\tif(s == 0) return 0;"
        Print !"\tint s_len = strlen(s);"
        Print !"\tdata = malloc(s_len);"
        Print !"\tlen = s_len;"
        Print !"\tsize = s_len;"
        Print !"\tmemcpy(data, s, s_len);"
        Print !"\treturn 0;"
        Print "}"

        Print "int FBSTRING::operator=(const FBSTRING& s)"
        Print "{"
        'print !"\tif(data) free(data);"
        Print !"\tdata = 0;"
        Print !"\tlen = 0;"
        Print !"\tsize = 0;"
        Print !"\tif(s.data == 0) return 0;"
        Print !"\tdata = malloc(s.len);"
        Print !"\tlen = s.len;"
        Print !"\tsize = s.len;"
        Print !"\tmemcpy(data, s.data, s.len);"
        Print !"\treturn 0;"
        Print "}"

        ' REL OP

        Print !"int FBSTRING::operator==(const FBSTRING& s)"
        Print "{"
        Print !"\tif(len != s.len) return 0;"
        Print !"\tif((data == 0) && (s.data == 0)) return 1;"
        Print !"\tif((data == 0) || (s.data == 0)) return 0;"
        Print !"\tif(memcmp(data, s.data, len) != 0) return 0;"
        Print !"\treturn 1;"
        Print "}"

        Print !"int FBSTRING::operator!=(const FBSTRING& s)"
        Print "{"
        Print !"\tif(len != s.len) return 1;"
        Print !"\tif((data == 0) && (s.data == 0)) return 0;"
        Print !"\tif((data == 0) || (s.data == 0)) return 1;"
        Print !"\tif(memcmp(data, s.data, len) != 0) return 1;"
        Print !"\treturn 0;"
        Print "}"

        ' SELF OP

        Print !"int FBSTRING::operator+=(const FBSTRING& s)"
        Print "{"
        Print !"\tFBSTRING result;"
        Print !"\tresult.data = data;"
        Print !"\tresult.len = len;"
        Print !"\tresult.size = size;"
        Print !"\tresult = concat(result, s);"
        Print !"\tfree(data);"
        Print !"\tdata = result.data;"
        Print !"\tlen = result.len;"
        Print !"\tsize = result.size;"
        Print !"\treturn 0;"
        Print "}"




        Print "void print(const FBSTRING& s, integer nl)"
        Print "{"
        Print !"\tint pos;"
        Print !"\tfor(pos = 0; pos < s.len; pos++) {"
        Print !"\t\tfputc(((char *)s.data)[pos], stdout);"
        Print !"\t}"
        Print !"\tif(nl) fputc(10, stdout);"
        Print "}"

        Print "FBSTRING& concat(const FBSTRING& s1, const FBSTRING& s2)"
        Print "{"
        Print !"\tFBSTRING *result = (FBSTRING *)calloc(sizeof(FBSTRING), 1);"
        Print !"\tresult->data = malloc(s1.len + s2.len);"
        Print !"\tresult->len = s1.len + s2.len;"
        Print !"\tresult->size = s1.len + s2.len;"
        Print !"\tmemcpy(result->data, s1.data, s1.len);"
        Print !"\tmemcpy(&(((char *)result->data)[s1.len]), s2.data, s2.len);"
        Print !"\treturn *result;"
        Print "}"

        Print "FBSTRING& lcase(const FBSTRING& s)"
        Print "{"
        Print !"\tFBSTRING *result = (FBSTRING *)calloc(sizeof(FBSTRING), 1);"
        Print !"\t*result = s;"
        Print !"\tint pos;"
        Print !"\tfor(pos = 0; pos < s.len; pos++) {"
        Print !"\t\t((char *)result->data)[pos] = tolower(((char *)s.data)[pos]);"
        Print !"\t}"
        Print !"\treturn *result;"
        Print "}"

        Print "FBSTRING& str_temp(const FBSTRING& s)"
        Print "{"
        Print !"\tFBSTRING *result = (FBSTRING *)calloc(sizeof(FBSTRING), 1);"
        Print !"\t*result = s;"
        Print !"\treturn *result;"
        Print "}"

        Print "FBSTRING& int_to_str(integer i)"
        Print "{"
        Print !"\tFBSTRING *result = (FBSTRING *)calloc(sizeof(FBSTRING), 1);"
        Print !"\tchar *tmp = (char *)calloc(32, 1);"
        Print !"\tsprintf(tmp, " & Chr( 34 ) & "%i" & Chr( 34 ) & ", i);"
        Print !"\tint tmp_len = strlen(tmp);"
        Print !"\tresult->data = tmp;"
        Print !"\tresult->len = tmp_len;"
        Print !"\tresult->size = 32;"
        Print !"\treturn *result;"
        Print "}"

        Print "FBSTRING& chr(integer i)"
        Print "{"
        Print !"\tFBSTRING *result = (FBSTRING *)calloc(sizeof(FBSTRING), 1);"
        Print !"\tresult->data = malloc(1);"
        Print !"\tresult->len = 1;"
        Print !"\tresult->size = 1;"
        Print !"\t((char *)result->data)[0] = i;"
        Print !"\treturn *result;"
        Print "}"

        Print "integer val(const FBSTRING& s)"
        Print "{"
        Print !"\tint n;"
        Print !"\tchar *tmp = (char *)calloc(s.len + 1, 1);"
        Print !"\tmemcpy(tmp, s.data, s.len);"
        Print !"\tn = atoi(tmp);"
        Print !"\tfree(tmp);"
        Print !"\treturn n;"
        Print "}"

        Print "integer asc(const FBSTRING& s)"
        Print "{"
        Print !"\tif(s.data == 0) return -1;"
        Print !"\treturn ((char *)s.data)[0];"
        Print "}"

        sym_add_proc( "lcase" )
        sym_add_proc( "exit_" )
        sym_add_proc( "EOF_" )
        sym_add_proc( "fgetc" )
        sym_add_proc( "asc" )
        sym_add_proc( "isdigit" )
        sym_add_proc( "isalnum" )
        sym_add_proc( "isalpha" )
        sym_add_proc( "callocate" )
        sym_add_proc( "sizeof" )
        sym_add_proc( "chr" )
        sym_add_proc( "val" )
        Dim As datatype_t Ptr dt = callocate( sizeof( datatype_t ) )
        dt->s = "integer"
        sym_add_dim( "__FUNCTION__", dt, 0, 0, -1 )
        sym_add_dim( "stdin", dt, 0, 0, -1 )

        read_char( )
        read_token( )
       
        parse_file( )
       
        If tk_typ <> TK_EOF Then
                Print "FAILED!"
        End If

        Function = 0

End Function

'::::::::
Rem main( __FB_ARGC__, __FB_ARGV__ )