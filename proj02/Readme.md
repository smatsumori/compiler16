# 第二回レポート
## 条件
1. コメントを扱えるようにする
コメントの形式は、/* hogehoge */のようにした。
2. エラーメッセージにエラー箇所のトークンを表示する
3. エラーメッセージに行番号を表示する
4. errorトークンを用いてエラー回復を実現する

## Implementing comments
Comments are specified as balancing /* and */.
`lexer.ml` will produce a lexical analyzer from a set of 
regular expressions, so in order to handle comment in lexer
you can do it by just adding regex to `lexer.mll`.

## エラーメッセージの表示
