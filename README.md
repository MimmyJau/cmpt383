# CMPT383

Self-study notes from the SFU's CMPT383.

Notes are [here](https://coursys.sfu.ca/2023su-cmpt-383-d1/pages/).

## Rainbow Table

To config, can modify these constants in `./rainbow.hs`.
```haskell
pwLength = 8
nLetters = 5
width = 40
height = 10000
filename = "table.txt"
```

To build:
```haskell
ghc --make -Wall ./rainbow.hs
```

To r
