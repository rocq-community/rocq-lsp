Load path flags passed with -arg in a _CoqProject reach the workspace

  $ export FCC_TEST=true
  $ mkdir dep proj

Build a library bound to the logical path Dep
  $ echo "-Q . Dep" > dep/_CoqProject
  $ echo "Definition d := 3." > dep/D.v
  $ fcc --display=quiet --root dep dep/D.v

Bind it again, this time through -arg
  $ echo "-R . Proj" > proj/_CoqProject
  $ echo "-arg \"-Q dep Dep\"" >> proj/_CoqProject
  $ echo "From Dep Require Import D. Check d." > proj/a.v
  $ fcc --display=quiet --root proj proj/a.v
  $ cat proj/a.diags
