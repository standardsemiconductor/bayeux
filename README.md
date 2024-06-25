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
  let zero = val (0 :: Word32)
  t <- process $ \timer -> do
    t1Sec <- timer === val 12000000
    timer' <- inc timer
    mux t1Sec timer' zero
  tNEqZ <- logicNot =<< t === zero
  c <- process $ \color -> do
    cEqBlue <- color === val Blue
    c' <- inc color
    ifm [ tNEqZ   `thenm` color
        , cEqBlue `thenm` val Red
        , elsem c'
        ]
  pwmR <- c === val Red
  pwmG <- c === val Green
  pwmB <- c === val Blue
  outputRgb pwmR pwmG pwmB
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

