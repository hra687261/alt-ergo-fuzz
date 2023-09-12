{ sources, lib, ocamlPackages }:

let
  dolmen = sources.dolmen;
in

ocamlPackages.buildDunePackage {
  strictDeps = true;
  pname = "dolmen";
  inherit (dolmen) version;

  minimalOCamlVersion = "4.08";
  duneVersion = "3";

  src = dolmen;

  nativeBuildInputs = [ ocamlPackages.menhir ];
  propagatedBuildInputs = [ ocamlPackages.menhirLib ocamlPackages.fmt ];

  meta = with lib; {
    inherit (dolmen) homepage description;
  };
}