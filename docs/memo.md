---
title: Memo
---

# Repairing λGT

グラフと型（生成文法）を入力にとり，
preprocessing したグラフと，
型が生成するグラフの集合を出力する．

## 方針

とりあえずは旧来のコードとは別に作る？

- gt (graph transformation) の実装は polymorphic に出来そうだが，
  どのみち free link と fusion の扱い方を変更する予定なので，
  ひとまず別に実装する．

グラフの定義．

- グラフの中では全て局所リンクということにして，
  外付けで局所リンクと自由リンク $(n \geq 0)$ の対応を持たせるのが良さそう？
  - $graph = atoms \times (FL \rightarrow LL)$
  - 自由リンクで置き換えられるものも局所リンクを経由しているので，
    局所リンクの数が必要な時は局所リンクの数から自由リンクの数を引いた値を用いる．
- int は type variable として扱う．
- production rule の適用：
  - 基本的には link substitution をして concat するだけ．
  - link substitution の方針：
    1. `x[Xs] -> RHS` とする．
    2. `Xs` から `RHS` の `link_env` への mapping を作っておく．
    3. 代入対象のグラフの variable が `y[Ys]` であるとき，
       `Xs -> Ys` の mapping を作る．
       ここで，`Xs` は全て free links, `Ys` は全て local links (link ids) である．
    4. `Xs -> Ys` の mapping で `RHS` の `link_env` を substitute する．

### Port Graph

グラフのノードは以下の 2 種類ある．

- アトム: Port を持つ頂点
  - それぞれの port からは hyper でない edge が出て，
    port または unnamed hyperlink に接続される．
- Hyperlink: Port を持たない頂点．普通のグラフ理論の頂点．
  - 任意本の edge が出て，
    port または hyperlink に接続される．

同じポートや hyperlink から多重辺が出ることは想定していない．
あくまでポートや hyperlink から出る辺は一本で，他のポートに直接繋がるか，
ハイパーリンクに接続されるかのどちらか．

# Todo

Fusion の扱い方を変えて，
free link 周りのグラフのマッチングを再実装する．

- グラフの中では全て局所リンクということにして，
  外付けで局所リンクと自由リンク $(n \geq 0)$ の対応を持たせるのが良さそう？
  - $graph = atoms \times (FL \rightarrow LL)$
- 自由リンクの集合は厳密にマッチする．
  - $G_1 \equiv G_2 \Rightarrow fn(G_1) = fn(G_2)$ なので（証明済み）．
- atom の matching は従来通り局所リンクの対応を作りながら行う．
  - 自由リンクの対応はする必要がない．
  - この対応は bijection とは限らない．
    何故なら graph context に fusion を補う可能性があるから．
- graph context の matching をもう少し考え直す．

# このリポジトリについて

このリポジトリは，
標準的なインタプリタと，
visualiser に渡すために JSON へ変換するコードと，
javascript として解釈実行するためのコードを含んでいる．

TODO: それぞれの使い方を整理してまとめる．

ディレクトリ構成

- bin: インタプリタを起動する，entry point.
- docs: ドキュメント．
- dune-project
- eval: evaluator
- gt: graph transformation を扱うコード．graph substitution も含む．
- example: 例題．
- js: vis のインタプリタを javascript から用いるためのコード．
- lib: 使っていない．全部この下に配置し直すべきかも．
- parser: parser.
- run: インタプリタを実行する shell script.
- scripts: 使い捨ての (shell) script などを置いておくためのディレクトリ．
- test: テストコード．ounit とかを使うようにするべきかも．
- util: 共用関数などを置いておくディレクトリ．base で置き換えられるかも．
- vis:
  - CPS で breakpoint から復帰できるようにしたインタプリタ．
  - graph を visualiser が扱いやすい形に変換して json で出力するコード．
