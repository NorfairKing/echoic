{ mkDerivation, base, brick, bytestring, lib, mtl, opt-env-conf
, path, text, typed-process, vty, vty-crossplatform
}:
mkDerivation {
  pname = "echoic";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base brick bytestring mtl opt-env-conf path text typed-process vty
    vty-crossplatform
  ];
  executableHaskellDepends = [ base ];
  description = "Blind computing environment";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
  mainProgram = "echoic";
}
