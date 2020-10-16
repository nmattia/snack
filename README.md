[![Build Status](https://travis-ci.org/nmattia/snack.svg?branch=master)](https://travis-ci.org/nmattia/snack)
[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

# Snack

**SNACK IS UNMAINTAINED!!**

_It was a fun proof of concept but I don't have the time to take it further._

_snack_ is a build tool that uses the power of Nix to build Haskell projects.

***Snack requires Nix >= 2.0***

It will

  * use your existing [Hpack][hpack] file or a Nix-based config (described
    [below](#hpack)).
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

## Install

_See the [Hacking](#hacking) section if you want to hack on snack_

Make sure you have [Nix][nix] installed.

### 1. With [niv]

1. If Niv is not already configured in your project:

``` shell
$ niv init
```

2. Add the `nmattia/snack` dependency with Niv (this will add an entry in your `nix/sources.json` with the latest revision of Snack):

``` shell
$ niv add nmattia/snack
```

3. Then to use it you will need to import the source description from `nix/sources.nix` and then import the source (which downloads and evaluates its `default.nix`) so you can access the `snack-exe` attribute:

``` nix
# nix/default.nix
let
  sources = import ./sources.nix;
in
  {
    snack = (import sources.snack).snack-exe;
  }
```

An example that combines this into an overlay over the `nixpkgs` also exported by `nix/sources.nix`:

``` nix
# nix/default.nix
{ sources ? import ./sources.nix }:
with
  { overlay = _: _:
      {
        niv = (import sources.niv {}).niv; # this is how you import niv into the overlay as well
        snack = (import sources.snack).snack-exe;
      };
  };
import sources.nixpkgs
  { overlays = [ overlay ] ; config = {}; }
```

4. Then you can have a `shell.nix` import it as such:

``` nix
# shell.nix
with { pkgs = import ./nix {}; };
pkgs.mkShell {
  buildInputs = [ pkgs.niv pkgs.nix pkgs.snack ];
}
```

### 2. Globally

Run this command:

``` shell
$ nix-env -iA snack-exe -f https://github.com/nmattia/snack/tarball/master
```

The _snack_ executable is now in your `PATH`:

``` shell
$ snack --help
Usage: <interactive> [-l|--lib DIR] ([-s|--snack-nix PATH] | [--no-snack-nix])
                     [-j|--jobs INT] [-p|--package-file PATH] (COMMAND |
                     COMMAND)

Available options:
  -l,--lib DIR             Path to the directory to use as the Nix library
                           instead of the default one bundled with the snack
                           executable.
  -s,--snack-nix PATH      Use the specified environment (snack.nix) file. When
                           none is provided ./snack.nix is used (if it exists).
                           (Use --no-snack-nix to disable this behavior)
  --no-snack-nix           Don't use ./snack.nix as the environment (snack.nix)
                           file.
  -j,--jobs INT            How many jobs to run concurrently
  -p,--package-file PATH   Specifies a YAML or Nix file to use as package
                           description. If not provided, snack looks for either
                           'package.yaml' or 'package.nix' in the current
                           directory.
  -h,--help                Show this help text

Available commands:
  build
  run
  ghci

Unavailable commands:
  test                     Use build, run or ghci commands with test suites.
```

Snack can be used to build, run and interact with packages. There is no
**test** command as we treat test suites as we do executables, giving each test
suite its own package description.

## Usage

There are two ways to describe a package:
* Use [`package.yaml` file](#hpack) for simple builds or if you already have
  a `package.yaml` file.
* Use a [`package.nix` file](#nix) if you need more control over your build.

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
$ snack run
```

You can also build without executing:

``` shell
$ snack build
```

Alternatively you can load up the project in `ghci`:

``` shell
$ snack ghci
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
$ snack run
```

Alternatively, use `$ snack build` or `$ snack ghci` if you only want to build,
or fire up `ghci`, respectively.

### Using other versions of GHC and nixpkgs

The _snack_ executable comes with a [bundled version of nixpkgs](./nix/sources.json) and uses the GHC executable provided
by `haskell.packages.ghc864`. You may override those defaults by
providing a `snack.nix`:

``` shell
# By default ./snack.nix is used if detected
$ snack build
# But you can also explicitly pass the snack file
$ snack --snack-nix my-snack.nix build
```

#### Customize the GHC version
``` nix
# snack.nix
rec {
  # If you only wish to change the version of GHC being used, set
  # `ghc-version`. The following versions are currently available:
  #  * ghc822
  #  * ghc822Binary
  #  * ghc844
  #  * ghc863Binary
  #  * ghc864
  #  * ghc865
  #  * ghcHEAD
  # NOTE: not all versions have been tested with snack.
  ghc-version = "ghc802";
}
```

#### Customize the `haskellPackages`
``` nix
# snack.nix
rec {
  # Alternatively you can provide your own `haskellPackages`, which should have
  # the same structure as that provided by
  # `pkgs.haskell.packages.<version>:
  haskellPackages = pkgs.haskell.packages.ghc822;
}
```

#### Customize the `pkgs`
``` nix
# snack.nix
rec {
  # Finally you can customize the whole nixpkgs used
  pkgs = import ./nix {};
}
```

### Advanced Nix Example

You may want custom builds that involve things such as [archiving and base64
encoding entire
directories](https://github.com/nmattia/snack/blob/c8e9e2d5ddaba2e0aa3e6c68a26bdc1063d387f3/bin/snack.nix#L10).

_snack_ builds itself, so its [`package.nix`](./bin/package.nix) is a good example
of an advanced configuration. You can also check out the [test
folder](./tests).

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
$ snack ghci -p ./bin/package.nix
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
*Main> :main ghci -l ./snack-lib/ -p ./tests/readme/package.nix
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
[niv]: https://github.com/nmattia/niv
