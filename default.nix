{ mkDerivation, base, pure, pure-lifted, pure-json, pure-txt, containers, stdenv }:
mkDerivation {
  pname = "pure-gestures";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [ base pure pure-lifted pure-json pure-txt containers ];
  homepage = "github.com/grumply/pure-gestures";
  description = "Basic gestures support";
  license = stdenv.lib.licenses.bsd3;
}
