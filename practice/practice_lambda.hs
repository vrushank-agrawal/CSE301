{-
    (λx.x) (λy.y)
    = x[x := λy.y]
    = λy.y
-}

{-
     (λx.λy.y)((λz.z)(λa.a))
     = (λx.λy.y)(z[z := (λa.a)]
     = (λx.λy.y)(λa.a)
     = (λy.y)[x := λa.a]
     = (λy.y)
-}

{-
    (λx.x)((λa.λb.ab)(λy.y)(λz.z))
    = x[x := ((λa.λb.ab)(λy.y)(λz.z))]
    = ((λa.λb.ab)(λy.y)(λz.z))
    = a[a := λy.y]b[b := λz.z]
    = (λy.y)(λz.z)
    = y[y := λz.z]
    = λz.z
-}

{-
    (λx.λy.λz.zyx)aa(λp.λq.q)
    = (λx.λy.z[z := (λp.λq.q)]yx)aa
    = (λx.(λp.λq.q) y[y := a] x)a
    = (λp.λq.q)a x[x := a]
    = (λp.λq.q)aa
    = q[q := a][p := a]
    = a
-}

{-
    (λx.λy.y)(λx.x)
    = (λy.y)[x := λx.x]
    = (λy.y)
-}

{-
    (λx.x x)(λy.y)
    = xx[x := (λy.y)]
    = (λy.y)(λy.y)
    = y[y := (λy.y)]
    = (λy.y)
-}

{-
    λa.(λx.λy.y x x)a(λb.b)
    = λa.(λx.λy.y x x)a(λb.b)
    = λa.y[y := (λb.b)]xx[x := a]
    = λa.(λb.b)aa
    = [a := (λb.b)aa]b[b := a]
    = a
-}