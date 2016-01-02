{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, conduit, containers
      , exceptions, file-embed, mtl, stdenv, template-haskell, text
      , transformers, xml-conduit, xml-conduit-writer, xml-types
      }:
      mkDerivation {
        pname = "wsdl";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          base bytestring conduit containers exceptions file-embed mtl
          template-haskell text transformers xml-conduit xml-conduit-writer
          xml-types
        ];
        license = stdenv.lib.licenses.gpl3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
