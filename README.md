# sush README
## 概要
R5RS準拠を目指すSchemeで書いたScheme処理系。大体R5RS準拠のつもりだけど、R5RSの仕様で不明瞭なところはR6RSに従う（予定）。

ミニマルで分かりやすい処理系であることを意図しており複雑な高速化は目指さない。最低限あんまり酷すぎはしない実装を行うことを目指す。

組み込み関数はR5RSで明示的に要求されている関数だけに留め、ライブラリ関数や特殊形式は処理系内部で関数およびマクロ定義を走らせることで定義。


## 内部データ構造
嘘だらけです

入力が評価されて、下記のオブジェクトが帰る
入力はプログラムを表現する通常のS式だが、マクロ展開時にマクロ定義側に現れた変数に関してマクロ定義の環境を保持する必要上、
該当するシンボルは('*hold-env* sym . env)の形に展開している。なので、'*hold-env*というシンボルをプログラム内で使うことはできない。

### オブジェクトの表現 (型シンボル . 内容)
型シンボルの種類
  boolean
  number
  char
  string
  symbol
  pair
  vector
  null

  procedure
  macro
  error
  undefined

procedureの内容の表現
  almost same as lambda form, except holdding env
    ('lambda env arg-pattern proc1 proc2 ...)
  builtin function
    ('builtin arg-pattern . meta-func)
  continuation
    ('continuation . stack)

macro
  almost same as syntax-rules
  (env literals rule1 rule2 ...)

identifier
  used for macro expansion
  (symbol . env)


### 構文実装について
lambda, syntax-rules, define, define-syntax, quote, quasiquoteをキーワードとして扱う。その他は、R5RSに載っているような感じに標準ライブラリ上でマクロで定義している。

ifは、if-funcという関数を定義した上で、マクロによるラムダ書き換えで評価を遅らせることで実装している。ifは構文無くてもマクロで出来るんじゃないか、ってのは江木さんの <http://is2008er.is-a-geek.org/index.php?Egi%2FScheme%2FMacro> 見て思い浮かんだ。

### スタック
実質的に継続のようなもの、こいつを末尾再帰で投げ続けることで処理系実装

TODO SECD！SECD！

((S . E) . (C . D))



スタックは以下のいずれかのリスト
それぞれidentifierは持たず、構造からどれに当てはまるか判定
argsはリスト

関数適用の際の関数の評価
  ~ ('func . args-not-evaled)
関数適用の際の引数の評価
  ~ ('arg func-evaled . (args-evaled-reversed . args-not-evaled))
call-by-valuesで作られた継続に突っ込むスタック
  ~ ('multi-arg . func-not-evaled)
その後の関数の評価に必要
  ~ ('multi-func . args-evaled)
lambdaのbodyの評価
  ~ ('body env . body-not-evaled)
defineで束縛される値の評価
  ~ ('define . symbol)

### マクロの制限
TODO これから無くす（コンパイルフェーズを作る、ユーザープログラムにエクスポートするシンボルと内部的なシンボルを明確に区別することによって）

ペア以外のものに展開されるマクロがかけない

  ~ set-car!, set-cdr!で展開された式を保持している都合上。マクロ形式の評価の都度展開させれば大丈夫だけど、それだとマクロと呼べるか怪しい。

syntax-rulesの中のテンプレート部にdefineなどを書くことでマクロ展開時に束縛された識別子を、テンプレート部に書かれた別のマクロ呼出しで使うと多分おかしくなる。

  ~ マクロが展開されたとき、マクロ呼出しに与えられたシンボルに由来する変数と、マクロ定義のテンプレートに由来する変数を区別しないといけない。

    この区別は環境をすり替えることで行なっているが、マクロが展開されたところでdefineされたときの処理も必要で、この場合はテンプレートに現れたシンボルをリネームして変数にすることで対処している。

    現状リネームは一階しか行えない。なので、テンプレート部で束縛した変数を別のマクロ呼出しに使ってしまうと変数の衝突が起こりうる。

マクロのテンプレート部でマクロ定義をすると多分おかしくなる

  ~ これも上の変数の区別の問題。こっちは簡単に回避できそうだけど、特に必要もないので。


## 泣き言
多値返しは意味的な綺麗さや実用性の割に面倒じゃないですかね。

あとマクロ内の変数がすごくこわかった。shidoさんのサイト <http://practical-scheme.net/wiliki/wiliki.cgi?Scheme%3A%E3%83%9E%E3%82%AF%E3%83%AD%3ACommonLisp%E3%81%A8%E3%81%AE%E6%AF%94%E8%BC%83> がとても役立った。環境を識別子に紐づけたものを評価させるように。パフォーマンスはあまり気にしない。というかマクロの展開書くのつらい。マクロつらい。つらい。つらかった。
