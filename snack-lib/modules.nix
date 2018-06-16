# module related operations
{ lib
, singleOut
}:

rec {
  # Turns a module name to a file
  moduleToFile = mod:
    (lib.strings.replaceChars ["."] ["/"] mod) + ".hs";

  # Turns a module name into the filepath of its object file
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
}
