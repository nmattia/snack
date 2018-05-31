[![Build Status](https://travis-ci.org/nmattia/snack.svg?branch=master)](https://travis-ci.org/nmattia/snack)
[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

# Snack

_snack_ is a Haskell build tool.

## Usage

You need a `snack.nix`:

``` nix
{ pkgs ? import <nixpkgs> {} }: # see #install for instructions
pkgs.snack-lib.executable
  { src = ./src; # Where you source code is located
    main = "Main"; # The name of your main module

    # You Haskell dependencies
    dependencies =
      [
        "heterocephalus"
        "servant"
        "servant-server"
        "warp"
        "unliftio"
        "uuid"
      ];

    # GHC options
    ghc-options = [ "-Wall" ];

    # Extra directories to add to your build, by module
    extra-directories = modName:
      if modName == "Main" then
      [ ./pages ]
      else [];
  }
```

You can then build with:

``` shell
$ snack build
```

or run with:

``` shell
$ snack run
> ...
```

or start an interactive session:
``` shell
$ snack ghci
> ...
```

Currently _snack_ only supports building executables.  See the [test
suite](./script/test) for examples.

## Install

The easiest way to install it is to add
it to your nix shell:

``` nix
{ pkgs ? import <nixpkgs> {} }:
let
  snack = (pkgs.callPackage path/to/snack {}).snack-exe;
in pkgs.mkShell
  { name = "snack-shell";
    buildInputs = [ snack ];
  }
```
