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
rgbCycle :: Monad m => MonadRtl m => MonadRgb m => m ()
rgbCycle = do
  t <- process 32 $ \timer -> do
    t1Sec  <- timer `eq` second
    timer' <- increment timer
    mux 32 t1Sec timer' zero
  tEqZ <- t `eq` zero
  c <- process 32 $ \color -> do
    cEqBlue <- color `eq` two
    c'      <- increment color
    mux 32 tEqZ color =<< mux 32 cEqBlue c' zero
  pwmR <- c `eq` zero
  pwmG <- c `eq` one
  pwmB <- c `eq` two
  rgb pwmR pwmG pwmB
  where
    constSig = SigSpecConstant . ConstantInteger
    zero   = constSig 0
    one    = constSig 1
    two    = constSig 2
    second = constSig 12000000
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

