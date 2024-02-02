# lexer execute
lexerのdebugはutop上で、

```
#use "parser.ml"
#use "lexer.ml"
let lexbuf = Lexing.from_string "int a,b,c;"
lexer lexbuf
```
# Next Task

front ex3において、エラーを戻すのなら、print_ast.ml のtry withが働かなくなってしまう。
- 変更: parser.mlyでposition取得 -> 直接ファイルからtokenを取ってくる

# 課題

## front-ex1
```
cd front-ex1
make print_ast
make print_lex
./print_lex sample_with_comment.spl
```

## front-ex2

### 概要

print_ast.ml 内で Lexing.lexeme を用いて情報を取得した

### 考えられる方法

エラー処理
- エラーが起こるとまず、parse_error が呼び出されるらしい。これは mlyファイル のheaderでカスタマイズできる
- https://v2.ocaml.org/manual/lexyacc.html
- https://github.com/pakt/ropc/blob/efac59364473533c5154410bfa7a079dd7efdcf8/bap-0.4/ocaml/grammar.mly#L105

エラーを起こした場所の取得
- Lexing.new_line で、lexbuf の中の行番号属性をカウントアップできるらしい。
- エラーを起こした場所のlexbuf情報はmlyでparse_errorを指定すればできる。

errorを起こしたsymbolの取得
- print_ast 側 の try with を用いれば、読み込み中のtoken が表示できるのでは？

結局 print_ast.ml 内で Lexing.lexeme を用いて情報を取得した


# ファイルの概要(自分向け)

## front-ex1/print_ast.ml
- lexの解析したtokenを表示する。
- 内部では、Lexer.lexer が raise End_of_file するまで try while true で読み込み
- ここでもyaccを使おうと思ったが、type宣言をlexerからyaccに書き写す必要があることからやめた

## front-ex2/print_ast.ml
- print_ast.ml 内で Lexing.lexeme を用いてエラー情報を得ている
- エラーはパーサにおけるParsing.errorとレキサにおけるLexer.No_such_symbolに対応している。処理は同じ
- 行情報であるpos_lnumを得るためには、lexer.mllでLexing.new_line lexbuf が必要


## front-ex3

lexerの時点で、各トークンにsymbolと行番号を付加して記録する。

errorが出たら、parserにおいて、reduceでsemanticsを参照するときにこれらのデータを取得する。


## back-ex1

ファイル構成
- ast.ml, parser.mly, lexer.mll は今まで通り
- types.ml: 二行しかない。tagとtyの定義
- table.ml: varInfo, funInfo, enventry 型が定義されている
- semant.ml: 
    - Ast, Types, Table をopenしている
- emitter.ml
    - アセンブリを出力する。
- sim.ml
    - 実行ファイルを作成する。だから短い
    
simc からの情報の流れ
- In sim.lm: 実行ファイル作成のための統括
lexbuf -> (Lexer.lexer Parser.prog) -> ast
-> (Emitter.trans_prog) -> code

- In Emitter.trans_prog
// Emitter.trans_stmt, Emitter.trans_dec がAST to Assembly
ast -> (trans_stmt x 0 initTable initTable) -> 

raxの符号拡張におけるエラーについて
- 様々なファイル上で乗算のコードをコピーして、剰余に変えたところ、
./back-ex1/sample_remainder.splにおいて、17 % 5 を実行すると、Answer = 1となった。
これはraxをrdx:raxにする時点でもともとrdxに何かが入っている可能性があると思い、
move $0 rdxを行うとこのエラーは解消された。詳しい原因はわからない。

 