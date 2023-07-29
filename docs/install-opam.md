# How to install opam

下記ステップを試してうまくいかなかったら教えてください．

備考

- 何か聞かれたら，全て Yes と答えて良い．
- 各パッケージのインストールは，それなりに時間がかかる（かも）

## Step 1. opam（OCaml Package Manager）をインストールする

ストレージに 1GB 以上の空きがある必要がある．

```sh
bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"
```

apt などだと，古いバージョンが入る可能性が高い．
公式ページの上記シェルスクリプトを実行するのが一番はやくて安心．

<https://opam.ocaml.org/doc/Install.html> を参照

この時点でおそらく OCaml のバージョンは 4.05.0 になっている．

- Step 2 でこれを更新する．

## Step 2. opam を利用して最新の OCaml をインストールする

現在最新の OCaml のバージョンは 4.14.0．
これに合わせる．

1. environment setup

```zsh
opam init
eval $(opam env)
```

2. install given version of the compiler

```zsh
opam switch create 4.14.0
eval $(opam env)
```

3. check you got what you want

```zsh
which ocaml     # ----> /your_home_directory/4.14.0/bin/ocaml
ocaml -version  # ----> The OCaml toplevel, version 4.14.0
```

apt などだと，古いバージョンが入る可能性が非常に高い．
opam でインストールするのが一番手っ取り早くて安心．

<https://ocaml.org/docs/install.html> を参照
