# _Bayeux_ 🥐
eHDL &#8743; prover
```
cabal build
cabal install
bx --help
```

#### <ins>Hardware</ins>

Build your chip
```haskell
cycleProg :: Monad m => MonadSignal m => MonadRgb m => m ()
cycleProg = do
  t <- process $ \timer -> patm timer
    [ 12000000 ~> val (0 :: Word32)
    , wildm $ inc timer
    ]
  c <- process $ \color -> patm t
    [ 12000000 ~> patm color
      [ Blue ~> val Red
      , wildm $ inc color
      ]
    , wildm $ pure color
    ]
  pwmR <- c === sig Red
  pwmG <- c === sig Green
  pwmB <- c === sig Blue
  outputRgb $ Sig $ spec pwmR <> spec pwmG <> spec pwmB
```

or synthesize a demo
```
bx --RgbCycle
```

#### <ins>Verification</ins>

Prove a propositional statement
```
echo "q => p => q" | bx --stdin -t
~(q => (p => q))
│
│q
│
│~(p => q)
│
│p
│
│~q

True
```

| Syntax | |
|------|-------------|
| Var   | alpha-num |
| Not   | `~`  |
| And   | `/\` |
| Or    | `\/` |
| Imply | `=>` |

