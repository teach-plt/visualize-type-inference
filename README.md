# visualize-type-inference

Visualize type inference for the simply-typed lambda-calculus
based on type meta variables and unification.

Usage:
```
visualize-type-inference [-b|--batch] [-J] [--no-colors] [FILE]

Available options:
  -b,--batch           Run in batch mode (rather than interactively step-by-step).
  -J                   Algorithm J: solve and substitute eagerly.
  --color when         Specify when colorized output should be generated.
                       Options: always, never, auto. Default: auto.
  FILE                 The text file containing the lambda-term for type inference.

  -V,--version         Show version info.
  --numeric-version    Show just version number.
  --license            Show the license text.
  -h,--help            Show help text.
```
By default, constraints are collected first and then solved one-by-one.
With option `-J`, each new constraint is solved right away.

Type inference is performed on a single term in Haskell syntax read from _FILE_.
If no _FILE_ is given, the term is read from standard input and processed in batch mode.

Example terms:
```haskell
\ f -> \ g -> \ x -> f (g x)
λ x → x x
```

BNFC grammar for terms:
```
EAbs.  Exp  ::= Lambda Ident Arrow Exp;
EApp.  Exp2 ::= Exp2 Exp3;
EId.   Exp3 ::= Ident;

coercions Exp 3;

token Lambda 'λ' | '\\';
token Arrow '→' | '-' '>';
```

Example runs from source repository:
```
stack run -- examples/two.txt
stack run -- -b <<< "λ x → x x"
```
