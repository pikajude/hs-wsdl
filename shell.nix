{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, conduit, exceptions, mtl
      , network-uri, resourcet, stdenv, text, xml-conduit, xml-types
      }:
      mkDerivation {
        pname = "wsdl";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          base bytestring conduit exceptions mtl network-uri resourcet text
          xml-conduit xml-types
        ];
        license = stdenv.lib.licenses.gpl3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
