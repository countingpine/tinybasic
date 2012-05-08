#include once "inc/sym.bi"

dim shared as integer TK_BAD     = 0
dim shared as integer TK_IDENT   = 256
dim shared as integer TK_LITINT  = 257
dim shared as integer TK_EOF     = 258
dim shared as integer TK_LITSTR  = 259
dim shared as integer TK_EOL     = 260
dim shared as integer TK_SELFADD = 261
dim shared as integer TK_NE      = 262
dim shared as integer TK_SELFSUB = 263
dim shared as integer TK_ARROW   = 264
dim shared as integer TK_GE      = 265

dim shared as integer look
dim shared as string  tk_str
dim shared as integer tk_typ
