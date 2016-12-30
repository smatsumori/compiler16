# 第二回レポート
## 条件
1. コメントを扱えるようにする
コメントの形式は、/* hogehoge */のようにした。
2. エラーメッセージにエラー箇所のトークンを表示する
3. エラーメッセージに行番号を表示する
4. errorトークンを用いてエラー回復を実現する

## Implementing comments
User defined comments are specified as balancing /* and */,
implemented by adding corresponding regular expression to lexer.mll
which will skip tokinzeing comment beforehand.

## Showing an error mesg
The comment implementation is done by handling an error in sim.ml
which is a main function in this program. 
Traking a line number and a token where the error occurs, are useful for
programmers to find what is wrong with the code.
Line numbers and tokens are defined by module Lexing.

## Error recovery
The pre-defined parsing-error only reports a parsing error and stops, therefore 
to handle the error and keep parsing the one of the possible ways is to use 
a special error symbols in grammar rules with a dummy action attatched to it.
