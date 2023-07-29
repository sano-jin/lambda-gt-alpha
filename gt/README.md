# Eval

Evaluator

- [syntax.ml](syntax.ml)

  - Syntax of atoms as an list.

- [preprocess.ml](preprocess.ml)

  - Transform an AST graph to a list of atoms. Alpha convert link names.

- [eval.ml](eval.ml)

  - The evaluator.

- [match.ml](match.ml)

  - Matches atoms and graph contexts and returns the obtained graph substitutions.

- [match_atoms.ml](match_atoms.ml)

  - Matches atoms and returns the link mappings and the rest graph.

- [postprocess.ml](postprocess.ml)

  - Transform the link names in the rest graphs and supply fusions
    according to the link mappings.

- [match_ctxs.ml](match.ml)

  - Matches graph contexts and returns the obtained graph substitutions.

- [pushout.ml](pushout.ml)

  - Substitute graph contexts with the given graph substitution (rewriting after matching).

## Matching Algorithm

`link_env` を用いてマッチングを行う．

- fusion をどのように扱うか？
  - local link の fusion は事前に吸収しておく．
  - free link の fusion, a quotient set of free link names, をどのように扱うか？
- host graph の free fusion が graph template の free fusion の subset でなければ，そもそもマッチングに失敗する．
  - host graph でもともと互いに素な自由リンクが，
    マッチングの過程で同じ同値類に属するようになるようなことはない．
  - 逆に，graph template で互いに素であったとしても，
    graph template 内の graph context に fusion を補えば，
    host graph で同じ同値類に属する自由リンクにマッチできる．
- 従って，host graph の free fusion が graph template の free fusion の subset (finer) であることを確認すれば良い．
  - マッチングの際に，非単射的であれば，quotient set を update する?

1. Check that the set of free link names of the host graph and the graph template are the same.
2. Check that the free fusion of the host graph is a finer quotient set than the free fusion of the graph template.
3. Match atoms updating `link_env`.
   - Free
