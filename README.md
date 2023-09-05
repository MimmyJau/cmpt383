# README

Self-studying SFU's [CMPT 383: Comparative Programming Languages](https://coursys.sfu.ca/2023su-cmpt-383-d1/pages/).

## Rainbow Table

Found in `./assignment1` directory.

To config, modify these constants in `./rainbow.hs`.
```haskell
pwLength = 8
nLetters = 5
width = 40
height = 10000
filename = "table.txt"
```

To build:
```console
ghc --make -Wall ./rainbow.hs
```
