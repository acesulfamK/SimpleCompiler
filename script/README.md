# lexer execute
lexerのdebugはutop上で、

```
#use "parser.ml"
#use "lexer.ml"
let lexbuf = Lexing.from_string "int a,b,c;"
lexer lexbuf
```

# ファイルの概要(自分向け)

front-ex1/print_ast.ml
- lexの解析したtokenを表示する。
- 内部では、Lexer.lexer が raise End_of_file するまで try while true で読み込み
- ここでもyaccを使おうと思ったが、type宣言をlexerからyaccに書き写す必要があることからやめた