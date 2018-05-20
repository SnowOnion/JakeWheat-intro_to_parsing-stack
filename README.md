# intro-to-parsing-stack

This repo is going to:

+ reorganize the programs in [Jake Wheat’s introduction to parsing with Haskell and Parsec](https://github.com/JakeWheat/intro_to_parsing) according to [Stack](https://www.haskellstack.org/)’s convention,

+ test all programs with latest GHC and [parsec](http://hackage.haskell.org/package/parsec) package.

<del>In this initial version, we use old `--resolver=lts-3.22` (GHC 7.10.2), `parsec == 3.1.9`, making the environment as similar to the author's as possible.</del>

Now we use `--resolver=lts-11.8` (GHC 8.2.2), `- parsec == 3.1.13.0`.
