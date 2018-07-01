let
  pkgs = import ./nix {};
  testSuite = name: path: pkgs.stdenv.mkDerivation
    { name = name;
      src = path;
      doCheck = true;
      checkPhase = "./test";
      installPhase = "touch $out";
      buildInputs = [ pkgs.snack-exe pkgs.nix capture_io pkgs.jq ];
    };
  capture_io = pkgs.writeScriptBin "capture_io"
    ''
OUT_FILE="$1"

cat <<END_HEREDOC
import GHC.IO.Handle
import System.IO
old_stdout <- hDuplicate stdout
:{
withFile "$OUT_FILE" WriteMode $ \\h -> do
    hDuplicateTo h stdout
    $2
    hDuplicateTo old_stdout stdout
:}
END_HEREDOC
    '';
in
{

  packages = testSuite "packages" ./tests/packages;
  th = testSuite "template-haskell" ./tests/template-haskell;
  th2 = testSuite "template-haskell2" ./tests/template-haskell-2;
  th3 = testSuite "template-haskell3" ./tests/template-haskell-3;
  th4 = testSuite "template-haskell4" ./tests/template-haskell-4;
  lib = testSuite "library" ./tests/library;
  lib2 = testSuite "library2" ./tests/library-2;

}
