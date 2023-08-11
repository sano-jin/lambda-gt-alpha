---
title: Memo
---

# Repairing λGT

グラフと型（生成文法）を入力にとり，
preprocessing したグラフと，
型が生成するグラフの集合を出力する．

現在，dlist において，`max_size` が 160 の場合に 6.82s.

- これ以上大きくするとスケールしない．
- プログラム中に記述するデータ構造のサイズという意味では，
  これくらいあれば十分という気はする．

## 方針

とりあえずは旧来のコードとは別に作る？

- gt (graph transformation) の実装は polymorphic に出来そうだが，
  どのみち free link と fusion の扱い方を変更する予定なので，
  ひとまず別に実装する．

### グラフの定義．

- グラフの中では全て局所リンクということにして，
  外付けで局所リンクと自由リンク $(n \geq 0)$ の対応を持たせるのが良さそう？
  - $graph = atoms \times (FL \rightarrow LL)$
  - 自由リンクで置き換えられるものも局所リンクを経由しているので，
    局所リンクの数が必要な時は局所リンクの数から自由リンクの数を引いた値を用いる．
- int などは type variable として扱う．

### Production rule の適用のアルゴリズム．

前提条件：

1. Production rule を
   $x[\overrightarrow{X}] \longrightarrow R$
   とする．
   1. ここで $\overrightarrow{X} = fn(R)$ である．
   2. $R$ の link_env は $X_i \mapsto {id}_i$
   3. $\overrightarrow{X}$ は全て free links である．
   4. $R$ の local links
      (自由リンク $\overrightarrow{X}$ に接続されている局所リンク $\overrightarrow{id}$ を含む)
      は，
      代入対象のグラフの local links とは干渉しないように unique にしておく．
2. 代入対象のグラフ中の variable が $y[\overrightarrow{Y}]$ であるとする．
   1. $\overrightarrow{Y}$ は全て local links (link ids) である．

アルゴリズム．

1. 前提として $R$ の local link ids は unique にしておいてある．
2. まず単に $R$ を concat する．
   - 前提より，concat 先のグラフのリンクと干渉しない．
3. $X_i \mapsto Y_i$ を作る．
4. $X_i \mapsto Y_i$ を用いて $R$ の link_env ($X_i \mapsto {id}_i$) の $X_i$ を $Y_i$ で置き換えて，
   $Y_i \mapsto {id}_i$ を作る．
5. concat 後のグラフ全体に対して， $Y_i \mapsto {id}_i$
   で fusion を行う．
   - fusion の実装は graph の preprocess を行う際に用いるので，既に済んでいる．

### Language generation の方針

POR したい．

- ルールにそれぞれ id を振る．
- 生成中の語と一緒に `(var_id * rule_id) set` を持っておいて，
  どの variable にどのルールを適用したかの set で比較する．
  - この集合が等しい時は同じ状態なので，追加しない．
  - 導出木の枝をずらすというので証明できそうな気がしてきた．

グラフのサイズ：

- 現在アトムの数と局所リンクの数と自由リンクの数で定義しているが，
  GED search にかける時のグラフのサイズを求めるべきなので，
  そのように変更する必要がある．

### Todo

整数アトムを int variable に変換する．

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

### グラフへの変換

アトムから hyperlink への辺は有向辺にして port 番号をラベルとして持たせる．

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
