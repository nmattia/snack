[![Build Status](https://travis-ci.org/nmattia/snack.svg?branch=master)](https://travis-ci.org/nmattia/snack)
[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

# Snack

_snack_ is a build tool that uses the power of Nix to build Haskell projects.

***Snack requires Nix >= 2.0***

It will

  * use your existing [Hpack][hpack] file or a Nix-based config (described
    [below](#nix)).
  * build your project incrementally: running `snack build` will only rebuild
    the modules that have been modified since the previous build.
  * work in the Nix sandbox.
  * give you lots of cool Nix features for free: strong reproducibility
    guarantees, remote caching, remote builds, and more.
  * improve build performance in some cases, for instance:
      - all Haskell modules are built in parallel.
      - there is a single linking step performed (typically) on a fast tmpfs.

Excited? Check out the [install](#install) and [usage](#usage) sections. Make
sure to also check out the [Caveat Emptor](#caveat-emptor) section.

## Why should I use Snack?

There are plenty of Haskell build tools around ([Cabal][cabal], [Stack][stack],
[Bazel][haskell-rules], ...). Unfortunately none of these allow what I consider
to be an ideal workflow:

1. The same build tool is used by developers and on CI.
2. The build tool guarantees that builds are reproducible.
3. The builds are incremental, i.e. if a library contains 300 modules and I
   modify the `main` function, only the `Main` module will be rebuilt.

Using Cabal inside of Nix solves (2); however this means that the builds are
not incremental anymore (3). This _may_ not be a problem on CI but definitely
is when developing locally. The way to work around that is to use Cabal inside
a nix-shell locally and call cabal2nix on CI. This means that developers use a
different tool locally than on CI (1). Moreover, a lot of projects nowadays use
Stack and, somewhat more importantly, Stackage LTSs. This makes local builds
quite easy (in spite of the occasional rebuild when changing flags) but in
order to perform a Nix build one has to generate some Nix boilerplate through
tools like stackage2nix or stack2nix (which do not always work on CI).

In comparison, _snack_ performs the exact same build on the developer's machine
as on CI. The builds are incremental, maybe more so than Cabal builds: if you
depend on a snack package _foo_ from package _bar_, and modify a module _Foo_
from _foo_ which isn't used in _bar_, no recompilation will occur. Moreover,
you benefit from your CI's cache. Finally, because _snack_ is just Nix (and
works with the Nix sandbox) you have pretty good guarantees that your builds
are reproducible.

### Caveat Emptor

The _snack_ library and executable are in their very early stages. They need a
lot of testing and massaging. The main (advertised) features are there, but (1)
may break for your particular project and (2) may break more in the future.

Now that this is out of the way, install _snack_, break it, and help me improve it!

## Install

_See the [Hacking](#hacking) section if you want to hack on snack_

Assuming that [Nix][nix] is installed on your machine, clone this repo
and run:

``` shell
$ nix-env -f ./default.nix -iA snack-exe
```

The _snack_ executable is now in your `PATH`:

``` shell
$ snack --help
Usage: snack [-l|--lib DIR] [-j|--cores INT] ([-s|--package-nix PATH] |
             [-p|--package-yaml PATH]) COMMAND

Available options:
  -l,--lib DIR             Path to the directory to use as the Nix library
                           instead of the default one bundled with the snack
                           executable.
  -j,--cores INT           How many cores to use during the build
  -h,--help                Show this help text

Available commands:
  build
  run
  ghci
```

## Usage

You can use Hpack (for simple builds or if you already have a `package.yaml`)
or Nix (if you need more control over your build).

The next two sections show an example config for each option. They use the
following example project which displays the title of the top-rated post on the
[haskell subreddit](https://www.reddit.com/r/haskell/) (you can also find the
code [here](tests/readme/)):

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

The project can have this minimal `package.yaml`:

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

This command will build the project and display the top-rated post's title:

``` shell
$ snack run --package-yaml ./package.yaml
```

You can also build without executing:

``` shell
$ snack build --package-yaml ./package.yaml
```

Alternatively you can load up the project in `ghci`:

``` shell
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

Building and running the project is as simple as

``` shell
$ snack run # looks for a file called package.nix by default
```

Alternatively, use `$ snack build` or `$ snack ghci` if you only want to build,
or fire up `ghci`, respectively.

### Advanced Nix Example


You may want custom builds that involve things such as [archiving and base64
encoding entire
directories](https://github.com/nmattia/snack/blob/c8e9e2d5ddaba2e0aa3e6c68a26bdc1063d387f3/bin/snack.nix#L10).

_snack_ builds itself, so its [`package.nix`](./bin/package.nix) is a good example
of an advanced configuration. You can also check out the [test
folder](./tests).

## Hacking

There are two different components you can hack:

* The snack executable in [`bin/Snack.hs`](./bin/Snack.hs)
* The snack library in [`snack-lib/`](./snack-lib)

Make sure you have a working version of snack installed, e.g.

``` shell
$ git co master
$ nix-env -f ./default.nix -iA snack-exe
```

If you are hacking on the _snack_ executable, just start _snack_ in a GHCi
session:

``` shell
$ snack ghci -s ./bin/package.nix
Temporarily symlinking /nix/store/j1x5vkxjr2ibabddfkdih4sm4kwinfda-spec-json/spec.json to spec.json...
done.
Temporarily symlinking /nix/store/w42y6dzgfmli9r8kmgh8akqk6kyda31x-lib64/lib.tar.gz.b64 to lib.tar.gz.b64...
done.
GHCi, version 8.2.2: http://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( /home/nicolas/projects/nmattia/snack/bin/Snack.hs, interpreted )
Ok, one module loaded.
*Main>
```

If you are hacking on the library, specify `-l/--lib` when running snack (this
works in GHCi too):

``` shell
*Main> :main ghci -l ./snack-lib/ -s ./tests/readme/package.nix
GHCi, version 8.2.2: http://www.haskell.org/ghc/  :? for help
[1 of 2] Compiling Lib              ( /home/nicolas/projects/nmattia/snack/tests/readme/src/Lib.hs, interpreted )
[2 of 2] Compiling Main             ( /home/nicolas/projects/nmattia/snack/tests/readme/app/Main.hs, interpreted )
Ok, two modules loaded.
*Main> :main
"\"Category Theory for Programmers\" has been finished!"
```

> Mustn't be afraid to dream a little bigger, darling.

## Thanks

Big thanks to

* [zimbatm](https://github.com/zimbatm) for brainstorming with me and improving
  the Nix code.
* [2mol](https://github.com/2mol) for showing me how to write understandable
  READMEs.
* quite a few people at ZuriHac for giving me ideas and feedback.
* whomever is willing to help, in advance.

[nix]: https://nixos.org/nix/
[hpack]: https://github.com/sol/hpack
[cabal]: https://www.haskell.org/cabal/
[stack]: https://haskellstack.org
[haskell-rules]: https://github.com/tweag/rules_haskell
