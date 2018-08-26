# file related operations
{ lib
, stdenv
, writeScript
}:

rec {
  # Takes a (string) filepath and creates a derivation for that file (and for
  # that file only)
  singleOut = base: file:
    let
      basePrefix = (builtins.toString base) + "/";
      pred = file: path: type:
        let
          actual = lib.strings.removePrefix basePrefix path;
          expected = file;
        in
          (expected == actual) ||
          (type == "directory" && (lib.strings.hasPrefix actual expected));
      # TODO: even though we're doing a lot of cleaning, there's sitll some
      # 'does-file-exist' happening
      src0 = lib.cleanSource base;
      name = # Makes the file name derivation friendly
        lib.stringAsChars
        (x:
          if x == "/" then "_"
          else if builtins.isNull (builtins.match "[a-zA-Z0-9.+=-_?]" x)
          then ""
          else x
        ) file;

    in stdenv.mkDerivation {
      inherit name;
      src = lib.cleanSourceWith  { filter = (pred file); src = src0; };
      builder = writeScript (name + "-single-out")
      # TODO: make sure the file actually exists and that there's only one
      ''
        echo "Singling out file ${file}"
        source $stdenv/setup
        mkdir -p $out
        echo "Running: cp $src/${file} $out/${file}"
        echo "Listing $src"
        ls $src/**/*
        mkdir -p $(dirname $out/${file})
        cp $src/${file} $out/${file}
        echo "Done: Singling out file ${file}"
      '';
    };

  doesFileExist = base: filename:
    lib.lists.elem filename (listFilesInDir base);

  fileExtension = fp:
    let
      exts = lib.splitString "." (builtins.baseNameOf fp);
    in if lib.length exts == 0 then null else lib.last exts;


  listFilesInDir = dir:
  let
    go = dir: dirName:
      lib.lists.concatLists
      (
        lib.attrsets.mapAttrsToList
          (path: ty:
            if ty == "directory"
            then
              go "${dir}/${path}" "${dirName}${path}/"
            else
              [ "${dirName}${path}" ]
          )
          (builtins.readDir dir)
      );
  in go dir "";
}
