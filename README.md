[![Build Status](https://travis-ci.org/nmattia/snack.svg?branch=master)](https://travis-ci.org/nmattia/snack)
[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

# Snack

_snack_ is a Haskell build tool.

## Usage

``` shell
$ $(nix-build)/out
```

See the [test suite](./script/test) for examples.

Example:

``` shell
$ ./script/test
$ ./tests/packages/result/out
1
2
3
4
5
```
