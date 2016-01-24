{ mkDerivation, base, bytestring, conduit, exceptions, file-embed
, hspec, mtl, network-uri, resourcet, stdenv, text, xml-conduit
, xml-types
}:
mkDerivation {
  pname = "wsdl";
  version = "0.1.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring conduit exceptions mtl network-uri resourcet text
    xml-conduit xml-types
  ];
  testHaskellDepends = [
    base bytestring file-embed hspec network-uri
  ];
  description = "WSDL parsing in Haskell";
  license = stdenv.lib.licenses.gpl3;
}
