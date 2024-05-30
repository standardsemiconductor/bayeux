# Bayeux

```
cabal build
cabal install
```
Try it:
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
