# module related operations
{ lib
, callPackage
, runCommand
, glibcLocales
, haskellPackages
}:

with (callPackage ./files.nix {});

rec {
  # Turns a module name to a file
  moduleToFile = mod:
    (lib.strings.replaceChars ["."] ["/"] mod) + ".hs";

  # Turns a module name into the filepath of its object file
  # TODO: bad name, this is module _name_ to object
  moduleToObject = mod:
    (lib.strings.replaceChars ["."] ["/"] mod) + ".o";

  # Turns a filepath name to a module name
  fileToModule = file:
    lib.strings.removeSuffix ".hs"
      (lib.strings.replaceChars ["/"] ["."] file);

  # Singles out a given module (by module name) (derivation)
  singleOutModule = base: mod: singleOut base (moduleToFile mod);

  # Singles out a given module (by module name) (path to module file)
  singleOutModulePath = base: mod:
    "${singleOut base (moduleToFile mod)}/${moduleToFile mod}";

  # Generate a list of haskell module names needed by the haskell file
  listModuleImports = baseByModuleName: modName:
    builtins.fromJSON
     (builtins.readFile (listAllModuleImportsJSON (baseByModuleName modName) modName))
    ;

  # Whether the file is a Haskell module or not. It uses very simple
  # heuristics: If the file starts with a capital letter, then yes.
  isHaskellModuleFile = f:
    ! (builtins.isNull (builtins.match "[a-zA-Z].*[.]hs$" (builtins.baseNameOf f)));

  listModulesInDir = dir:
    map fileToModule
      (lib.filter isHaskellModuleFile
      (listFilesInDir dir));

  doesModuleExist = baseByModuleName: modName:
    doesFileExist (baseByModuleName modName) (moduleToFile modName);

  # Lists all module dependencies, not limited to modules existing in this
  # project
  listAllModuleImportsJSON = base: modName:
    let
      ghc = haskellPackages.ghcWithPackages (ps: [ ps.ghc ]);
      importParser = runCommand "import-parser"
        { buildInputs = [ ghc ];
        } "ghc -Wall -Werror -package ghc ${./Imports.hs} -o $out" ;
    # XXX: this command needs ghc in the environment so that it can call "ghc
    # --print-libdir"...
    in runCommand "dependencies-json"
      {   buildInputs = [ ghc glibcLocales ];
          LANG="en_US.utf-8";
      }

        ''
          ${importParser} ${singleOutModulePath base modName} > $out
        '';
}
