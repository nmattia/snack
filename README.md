[![Build Status](https://travis-ci.org/nmattia/snack.svg?branch=master)](https://travis-ci.org/nmattia/snack)
[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

# Snack

_snack_ is a build tool that uses the power of Nix to build Haskell projects.
It will

  * use your existing [Hpack][hpack] file or a Nix-based config (described
    [below](#nix)).
  * build your project incrementally: running `snack build` will only rebuild
    the modules that have been modified since the previous build.
  * work in the Nix sandbox.
  * give you lots of cool Nix features for free: strong reproducibility
    guarantees, remote caching, remote builds, and more.
  * improve build performance in some cases, for instance:
      - all Haskell modules modules are built in parallel.
      - there is a single linking step performed (typically) on a fast tmpfs.

## Install

Assuming that [Nix][nix] is installed on your machine, clone this repo
and run:

``` shell
$ nix-env -f ./default.nix -iA snack-exe
installing 'linker'
```

The _snack_ executable is now in your `PATH`:

``` shell
$ snack --help
Usage: snack ([-s|--snack-nix PATH] | [-p|--package-yaml PATH]) COMMAND

Available options:
  -h,--help                Show this help text

Available commands:
  build
  run
  ghci
```

## Usage

You can use Hpack -- for simple builds or if you already have a `package.yaml`
-- or Nix -- if you need more control over your build.

The next two sections show an example config for each option. They use the
following example project (which you can also find [here](tests/readme/)):

```shell
.
├── app
│   └── Main.hs
└── src
    └── Lib.hs
```

***src/Lib.hs*** :
``` haskell
module Lib where

import Control.Lens
import Network.Wreq
import Data.Aeson.Lens
import Data.Text (Text)

topReddit :: IO Text
topReddit =
    getWith opts url
      <&> (^. responseBody
      . key "data"
      . key "children"
      . nth 0
      . key "data"
      . key "title"
      . _String)
  where
    url = "https://www.reddit.com/r/haskell/top.json"
    opts = defaults
      & param "limit" .~ ["1"]
      & param "t" .~ ["all"]
```

***app/Main.hs*** :
``` haskell
module Main where

import Lib

main :: IO ()
main = topReddit >>= print
```

### Hpack

The project can have this minmal `package.yaml`:

``` yaml
name: snack-readme

dependencies:
    - lens
    - wreq

library:
    source-dirs: ./src

executable:
    main: Main.hs
    source-dirs: ./app
    dependencies:
        - snack-readme

default-extensions:
    - OverloadedStrings
```

This command will build and run the project:

``` shell
$ snack run --package-yaml ./package.yaml
```

You can also build without executing the resulting program:

``` shell
$ snack build --package-yaml ./package.yaml
```

You can also build and load up the result in `ghci`:

```
$ snack ghci --package-yaml ./package.yaml
GHCi, version 8.2.2: http://www.haskell.org/ghc/  :? for help
[1 of 2] Compiling Lib              ( /home/nicolas/projects/nmattia/snack/tests/readme/src/Lib.hs, interpreted )
[2 of 2] Compiling Main             ( /home/nicolas/projects/nmattia/snack/tests/readme/app/Main.hs, interpreted )
Ok, two modules loaded.
*Main>
```

### Nix


To build the project the following Nix config is sufficient:

``` nix
let
  lib =
    { src = ./src;
      dependencies = [ "wreq" "lens" ];
      extensions = [ "OverloadedStrings"];
    };
in
  { main = "Main";
    src = ./app;
    packages = [ lib ];
    dependencies = [ "wreq" "lens" ];
  }
```

To build and run the project is as simple as

``` shell
$ snack run
```

Alternatively, use `$ snack build` or `$ snack ghci` if you only want to build,
or fire up `ghci`, respectively.

### Advanced Nix Example


You may want custom builds that involve things such as archiving and base64
encoding entire directories.

_snack_ builds itself, so its configuration file is a very good example:
[_snack_'s `snack.nix`](./bin/snack.nix).


[nix]: https://nixos.org/nix/
[hpack]: https://github.com/sol/hpack
