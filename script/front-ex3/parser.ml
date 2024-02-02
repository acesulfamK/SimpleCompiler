type token =
  | NUM of (int)
  | STR of (string)
  | ID of (string)
  | INT
  | IF
  | WHILE
  | SPRINT
  | IPRINT
  | SCAN
  | EQ
  | NEQ
  | GT
  | LT
  | GE
  | LE
  | ELSE
  | RETURN
  | NEW
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | LB
  | RB
  | LS
  | RS
  | LP
  | RP
  | ASSIGN
  | SEMI
  | COMMA
  | TYPE
  | VOID
  | ERROR

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"

open Printf
open Ast
let parse_error str = 
     let {Lexing.pos_fname=sf; pos_lnum=sl; pos_bol=sb; pos_cnum=sc} =
               Parsing.symbol_start_pos() in
               Printf.eprintf "%s:line %d:pos %d:error\n" 
                    sf sl (sc-sb) 

let print_error lexbuf =
   let split_before_space str =
       try
           let i = String.index str ' ' in
           String.sub str 0 i
       with
       | Not_found -> str
   in
   Printf.printf "%s\n" (split_before_space (Lexing.lexeme lexbuf))
# 59 "parser.ml"
let yytransl_const = [|
  260 (* INT *);
  261 (* IF *);
  262 (* WHILE *);
  263 (* SPRINT *);
  264 (* IPRINT *);
  265 (* SCAN *);
  266 (* EQ *);
  267 (* NEQ *);
  268 (* GT *);
  269 (* LT *);
  270 (* GE *);
  271 (* LE *);
  272 (* ELSE *);
  273 (* RETURN *);
  274 (* NEW *);
  275 (* PLUS *);
  276 (* MINUS *);
  277 (* TIMES *);
  278 (* DIV *);
  279 (* LB *);
  280 (* RB *);
  281 (* LS *);
  282 (* RS *);
  283 (* LP *);
  284 (* RP *);
  285 (* ASSIGN *);
  286 (* SEMI *);
  287 (* COMMA *);
  288 (* TYPE *);
  289 (* VOID *);
  290 (* ERROR *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* STR *);
  259 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\003\000\003\000\003\000\004\000\004\000\005\000\005\000\
\005\000\005\000\006\000\006\000\007\000\007\000\009\000\009\000\
\010\000\010\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\013\000\013\000\014\000\014\000\008\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\012\000\012\000\012\000\012\000\012\000\012\000\000\000"

let yylen = "\002\000\
\001\000\001\000\004\000\001\000\002\000\000\000\003\000\005\000\
\006\000\006\000\003\000\001\000\000\000\001\000\004\000\002\000\
\002\000\001\000\004\000\007\000\005\000\003\000\001\000\007\000\
\005\000\003\000\005\000\005\000\005\000\005\000\005\000\003\000\
\001\000\001\000\000\000\001\000\003\000\001\000\004\000\001\000\
\001\000\004\000\004\000\003\000\003\000\003\000\003\000\002\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\023\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\006\000\034\000\056\000\001\000\033\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\040\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\022\000\000\000\
\000\000\026\000\000\000\000\000\000\000\000\000\000\000\000\000\
\048\000\000\000\000\000\000\000\000\000\000\000\032\000\000\000\
\000\000\000\000\000\000\000\000\018\000\000\000\005\000\000\000\
\000\000\000\000\000\000\019\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\049\000\000\000\000\000\046\000\047\000\000\000\000\000\
\000\000\000\000\000\000\000\000\039\000\017\000\000\000\031\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\025\000\027\000\028\000\029\000\043\000\042\000\030\000\000\000\
\000\000\000\000\000\000\007\000\000\000\000\000\000\000\003\000\
\004\000\000\000\000\000\000\000\000\000\000\000\011\000\020\000\
\024\000\008\000\016\000\000\000\000\000\000\000\010\000\000\000\
\009\000\015\000"

let yydgoto = "\002\000\
\014\000\015\000\123\000\033\000\063\000\092\000\124\000\016\000\
\125\000\064\000\035\000\041\000\036\000\037\000"

let yysindex = "\033\000\
\136\255\000\000\000\000\004\255\003\255\005\255\012\255\029\255\
\038\255\018\255\051\255\000\000\000\000\000\000\000\000\000\000\
\018\255\018\255\018\255\136\255\018\255\136\255\018\255\087\255\
\018\255\088\255\000\000\237\254\018\255\018\255\033\255\104\255\
\079\255\247\254\040\255\080\255\093\255\084\255\000\000\216\255\
\097\255\000\000\098\255\102\255\053\255\107\255\018\255\018\255\
\000\000\127\255\018\255\018\255\018\255\018\255\000\000\109\255\
\004\255\113\255\126\255\129\255\000\000\147\255\000\000\110\255\
\122\255\128\255\018\255\000\000\018\255\018\255\018\255\018\255\
\018\255\018\255\136\255\136\255\137\255\138\255\143\255\021\255\
\124\255\000\000\002\255\002\255\000\000\000\000\144\255\155\255\
\146\255\130\255\152\255\046\255\000\000\000\000\018\255\000\000\
\040\255\040\255\040\255\040\255\040\255\040\255\040\255\161\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\162\255\
\091\255\091\255\091\255\000\000\186\255\101\255\136\255\000\000\
\000\000\160\255\190\255\166\255\164\255\168\255\000\000\000\000\
\000\000\000\000\000\000\174\255\091\255\174\255\000\000\196\255\
\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\173\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\150\255\000\000\000\000\000\000\000\000\
\000\000\000\000\253\254\000\000\182\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\173\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\208\255\006\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\172\255\194\255\000\000\000\000\000\000\000\000\
\000\000\000\000\068\255\000\000\000\000\000\000\000\000\000\000\
\062\255\184\255\187\255\188\255\189\255\191\255\193\255\001\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\195\255\195\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\204\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\238\255\223\255\000\000\000\000\000\000\103\000\159\255\
\000\000\000\000\253\255\210\000\186\000\000\000"

let yytablesize = 287
let yytable = "\062\000\
\021\000\039\000\020\000\042\000\022\000\047\000\031\000\048\000\
\002\000\051\000\052\000\053\000\054\000\034\000\061\000\038\000\
\065\000\040\000\027\000\040\000\028\000\045\000\053\000\054\000\
\038\000\049\000\050\000\038\000\017\000\021\000\018\000\023\000\
\019\000\001\000\135\000\002\000\137\000\029\000\024\000\051\000\
\052\000\053\000\054\000\080\000\030\000\094\000\109\000\083\000\
\084\000\085\000\086\000\051\000\052\000\053\000\054\000\025\000\
\104\000\105\000\051\000\052\000\053\000\054\000\055\000\097\000\
\026\000\098\000\099\000\100\000\101\000\102\000\103\000\051\000\
\052\000\053\000\054\000\116\000\117\000\032\000\003\000\122\000\
\078\000\057\000\058\000\005\000\006\000\007\000\008\000\009\000\
\044\000\037\000\046\000\118\000\037\000\121\000\058\000\010\000\
\011\000\012\000\012\000\136\000\129\000\012\000\051\000\052\000\
\053\000\054\000\056\000\066\000\013\000\003\000\059\000\060\000\
\004\000\068\000\005\000\006\000\007\000\008\000\009\000\051\000\
\052\000\053\000\054\000\067\000\075\000\076\000\010\000\011\000\
\089\000\077\000\128\000\090\000\012\000\093\000\079\000\003\000\
\087\000\088\000\004\000\013\000\005\000\006\000\007\000\008\000\
\009\000\051\000\052\000\053\000\054\000\091\000\095\000\110\000\
\010\000\011\000\082\000\112\000\114\000\096\000\012\000\041\000\
\041\000\041\000\041\000\041\000\041\000\013\000\106\000\107\000\
\041\000\041\000\041\000\041\000\108\000\111\000\113\000\041\000\
\119\000\041\000\115\000\041\000\041\000\044\000\044\000\044\000\
\044\000\044\000\044\000\120\000\127\000\130\000\044\000\044\000\
\131\000\132\000\133\000\134\000\012\000\044\000\138\000\044\000\
\035\000\044\000\044\000\045\000\045\000\045\000\045\000\045\000\
\045\000\036\000\004\000\050\000\045\000\045\000\051\000\052\000\
\053\000\126\000\054\000\045\000\055\000\045\000\013\000\045\000\
\045\000\069\000\070\000\071\000\072\000\073\000\074\000\014\000\
\043\000\081\000\051\000\052\000\053\000\054\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\021\000\000\000\000\000\021\000\000\000\021\000\021\000\021\000\
\021\000\021\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\021\000\021\000\000\000\000\000\000\000\000\000\021\000\
\021\000\000\000\000\000\000\000\000\000\000\000\021\000"

let yycheck = "\033\000\
\000\000\020\000\000\001\022\000\000\001\025\001\010\000\027\001\
\003\001\019\001\020\001\021\001\022\001\017\000\033\000\019\000\
\026\001\021\000\001\001\023\000\003\001\025\000\021\001\022\001\
\028\001\029\000\030\000\031\001\025\001\027\001\027\001\027\001\
\029\001\001\000\132\000\030\001\134\000\020\001\027\001\019\001\
\020\001\021\001\022\001\047\000\027\001\064\000\026\001\051\000\
\052\000\053\000\054\000\019\001\020\001\021\001\022\001\027\001\
\075\000\076\000\019\001\020\001\021\001\022\001\030\001\067\000\
\027\001\069\000\070\000\071\000\072\000\073\000\074\000\019\001\
\020\001\021\001\022\001\030\001\031\001\027\001\000\001\113\000\
\028\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\002\001\028\001\003\001\095\000\031\001\003\001\004\001\017\001\
\018\001\030\001\031\001\133\000\119\000\023\001\019\001\020\001\
\021\001\022\001\003\001\028\001\030\001\000\001\032\001\033\001\
\003\001\030\001\005\001\006\001\007\001\008\001\009\001\019\001\
\020\001\021\001\022\001\031\001\028\001\028\001\017\001\018\001\
\003\001\028\001\030\001\003\001\023\001\024\001\028\001\000\001\
\028\001\025\001\003\001\030\001\005\001\006\001\007\001\008\001\
\009\001\019\001\020\001\021\001\022\001\003\001\029\001\028\001\
\017\001\018\001\028\001\001\001\027\001\030\001\023\001\010\001\
\011\001\012\001\013\001\014\001\015\001\030\001\030\001\030\001\
\019\001\020\001\021\001\022\001\030\001\030\001\029\001\026\001\
\016\001\028\001\027\001\030\001\031\001\010\001\011\001\012\001\
\013\001\014\001\015\001\026\001\003\001\030\001\019\001\020\001\
\003\001\028\001\031\001\028\001\023\001\026\001\003\001\028\001\
\028\001\030\001\031\001\010\001\011\001\012\001\013\001\014\001\
\015\001\028\001\003\001\028\001\019\001\020\001\028\001\028\001\
\028\001\115\000\028\001\026\001\028\001\028\001\028\001\030\001\
\031\001\010\001\011\001\012\001\013\001\014\001\015\001\028\001\
\023\000\048\000\019\001\020\001\021\001\022\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\000\001\255\255\255\255\003\001\255\255\005\001\006\001\007\001\
\008\001\009\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\017\001\018\001\255\255\255\255\255\255\255\255\023\001\
\024\001\255\255\255\255\255\255\255\255\255\255\030\001"

let yynames_const = "\
  INT\000\
  IF\000\
  WHILE\000\
  SPRINT\000\
  IPRINT\000\
  SCAN\000\
  EQ\000\
  NEQ\000\
  GT\000\
  LT\000\
  GE\000\
  LE\000\
  ELSE\000\
  RETURN\000\
  NEW\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  LB\000\
  RB\000\
  LS\000\
  RS\000\
  LP\000\
  RP\000\
  ASSIGN\000\
  SEMI\000\
  COMMA\000\
  TYPE\000\
  VOID\000\
  ERROR\000\
  "

let yynames_block = "\
  NUM\000\
  STR\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 41 "parser.mly"
             (  _1  )
# 310 "parser.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 44 "parser.mly"
           ( IntTyp )
# 316 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 45 "parser.mly"
                     ( ArrayTyp (_3, IntTyp) )
# 323 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 46 "parser.mly"
               ( NameTyp _1 )
# 330 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decs) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'dec) in
    Obj.repr(
# 49 "parser.mly"
                ( _1@_2 )
# 338 "parser.ml"
               : 'decs))
; (fun __caml_parser_env ->
    Obj.repr(
# 50 "parser.mly"
                ( [] )
# 344 "parser.ml"
               : 'decs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ids) in
    Obj.repr(
# 53 "parser.mly"
                     ( List.map (fun x -> VarDec (_1,x)) _2 )
# 352 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 54 "parser.mly"
                              ( [TypeDec (_2,_4)] )
# 360 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'ty) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'fargs_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 55 "parser.mly"
                                    ( [FuncDec(_2, _4, _1, _6)] )
# 370 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'fargs_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 56 "parser.mly"
                                      ( [FuncDec(_2, _4, VoidTyp, _6)] )
# 379 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ids) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 59 "parser.mly"
                       ( _1@[_3] )
# 387 "parser.ml"
               : 'ids))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 60 "parser.mly"
                       ( [_1]  )
# 394 "parser.ml"
               : 'ids))
; (fun __caml_parser_env ->
    Obj.repr(
# 63 "parser.mly"
                        ( [] )
# 400 "parser.ml"
               : 'fargs_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fargs) in
    Obj.repr(
# 64 "parser.mly"
                        ( _1 )
# 407 "parser.ml"
               : 'fargs_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'fargs) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 67 "parser.mly"
                             ( _1@[(_3,_4)] )
# 416 "parser.ml"
               : 'fargs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 68 "parser.mly"
                             ( [(_1,_2)] )
# 424 "parser.ml"
               : 'fargs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 71 "parser.mly"
                   ( _1@[_2] )
# 432 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 72 "parser.mly"
                   ( [_1] )
# 439 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 75 "parser.mly"
                              ( Assign (Var _1, _3) )
# 447 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 76 "parser.mly"
                                       ( Assign (IndexedVar (Var _1, _3), _6) )
# 456 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'cond) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 77 "parser.mly"
                              ( If (_3, _5, None) )
# 464 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 78 "parser.mly"
                              ( If (IntExp(0), _3, None))
# 471 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "parser.mly"
                              ( print_error lexbuf;ERROR)
# 477 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'cond) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 81 "parser.mly"
                              ( If (_3, _5, Some _7) )
# 486 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'cond) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 82 "parser.mly"
                              ( While (_3, _5) )
# 494 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 83 "parser.mly"
                              ( While (IntExp(0), _3) )
# 501 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 84 "parser.mly"
                              ( CallProc ("sprint", [StrExp _3]) )
# 508 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    Obj.repr(
# 85 "parser.mly"
                              ( CallProc ("iprint", [_3]) )
# 515 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 86 "parser.mly"
                           ( CallProc ("scan", [VarExp (Var _3)]) )
# 522 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 87 "parser.mly"
                           ( CallProc ("new", [ VarExp (Var _3)]) )
# 529 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'aargs_opt) in
    Obj.repr(
# 88 "parser.mly"
                                ( CallProc (_1, _3) )
# 537 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 89 "parser.mly"
                           ( CallProc ("return", [_2]) )
# 544 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 90 "parser.mly"
             ( _1 )
# 551 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 91 "parser.mly"
            ( NilStmt )
# 557 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "parser.mly"
                           ( [] )
# 563 "parser.ml"
               : 'aargs_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'aargs) in
    Obj.repr(
# 95 "parser.mly"
                           ( _1 )
# 570 "parser.ml"
               : 'aargs_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aargs) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 98 "parser.mly"
                          ( _1@[_3] )
# 578 "parser.ml"
               : 'aargs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 99 "parser.mly"
                           ( [_1] )
# 585 "parser.ml"
               : 'aargs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'decs) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 102 "parser.mly"
                         ( Block (_2, _3) )
# 593 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 105 "parser.mly"
           ( IntExp _1  )
# 600 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 106 "parser.mly"
          ( VarExp (Var _1) )
# 607 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'aargs_opt) in
    Obj.repr(
# 107 "parser.mly"
                          ( CallFunc (_1, _3) )
# 615 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 108 "parser.mly"
                      ( VarExp (IndexedVar (Var _1, _3)) )
# 623 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 109 "parser.mly"
                      ( CallFunc ("+", [_1; _3]) )
# 631 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 110 "parser.mly"
                       ( CallFunc ("-", [_1; _3]) )
# 639 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 111 "parser.mly"
                       ( CallFunc ("*", [_1; _3]) )
# 647 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 112 "parser.mly"
                     ( CallFunc ("/", [_1; _3]) )
# 655 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 113 "parser.mly"
                               ( CallFunc("!", [_2]) )
# 662 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 114 "parser.mly"
                   ( _2 )
# 669 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 117 "parser.mly"
                     ( CallFunc ("==", [_1; _3]) )
# 677 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 118 "parser.mly"
                     ( CallFunc ("!=", [_1; _3]) )
# 685 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 119 "parser.mly"
                     ( CallFunc (">", [_1; _3]) )
# 693 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 120 "parser.mly"
                     ( CallFunc ("<", [_1; _3]) )
# 701 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 121 "parser.mly"
                     ( CallFunc (">=", [_1; _3]) )
# 709 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 122 "parser.mly"
                     ( CallFunc ("<=", [_1; _3]) )
# 717 "parser.ml"
               : 'cond))
(* Entry prog *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let prog (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.stmt)
;;
