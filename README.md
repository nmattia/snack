[![Build Status](https://travis-ci.org/nmattia/snack.svg?branch=master)](https://travis-ci.org/nmattia/snack)
[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

# Snack

_snack_ is a build tool which

  * uses Nix to build Haskell projects (HPack and special Nix configuration)
  * is incremental: running `snack build` will only rebuild the modules that
    have been modified since the previous build
  * works in the Nix sandbox
  * comes with lots of cool Nix features for free: strong reproducibility
    guarantees, remote caching, remote builds, and more
  * may improve build performance in some cases, for instance:
      - all Haskell modules modules are built in parallel
      - there is a single linking step performed (typically) on a fast tmpfs

## Install

You need [nix][nix]. Then, clone this repo and run:

``` shell
$ nix-env -f ./default.nix -iA snack-exe
installing 'linker'
```

The _snack_ executable should now be available:

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

_snack_ uses HPack for building. If you need more power in your builds, you can
also use raw nix. The next sections will give example HPack and Nix
configurations for building the following project:

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

### HPack

_snack_ does not purport to support HPack entirely (yet) but merely uses some
heuristics that map relatively well to HPack's specification. The
aforementioned project could have the following `package.yaml`:


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

The project can be run by using the following command:

``` shell
$ snack run --package-yaml ./package.yaml
"\"Category Theory for Programmers\" has been finished!"
```


You may decide to simply build the project, or even start an interactive
session:

``` shell
$ snack build --package-yaml ./package.yaml

/nix/store/x3ahgw45qja93jrbm2qd3pywfy2zcq9c-hpack-build-json

$ snack ghci --package-yaml ./package.yaml
GHCi, version 8.2.2: http://www.haskell.org/ghc/  :? for help
[1 of 2] Compiling Lib              ( /home/nicolas/projects/nmattia/snack/tests/readme/src/Lib.hs, interpreted )
[2 of 2] Compiling Main             ( /home/nicolas/projects/nmattia/snack/tests/readme/app/Main.hs, interpreted )
Ok, two modules loaded.
*Main>
```

### Nix


Here is the equivalent Nix config :

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

The same commands can then be used:

``` shell
$ snack --snack-nix ./snack.nix run
"\"Category Theory for Programmers\" has been finished!"

```

Note that the `--snack-nix` argument can be omitted if a `./snack.nix` file is
present. Using a Nix (as opposed to HPack) for building your projects gives you
a lot of power and flexibility. For instance see [_snack_'s
`snack.nix`](./bin/snack.nix) (yes, snack builds itself!).


[nix]: https://nixos.org/nix/
