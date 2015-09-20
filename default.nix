{ mkDerivation, base, ghc-prim, stdenv }:
mkDerivation {
  pname = "x24-bit";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ghc-prim ];
  license = stdenv.lib.licenses.mit;
}
