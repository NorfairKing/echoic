{ mkDerivation, async, base, brick, bytestring, containers, cursor
, cursor-brick, lib, mtl, opt-env-conf, path, stm, text
, typed-process, vty, vty-crossplatform
}:
mkDerivation {
  pname = "echoic";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async base brick bytestring containers cursor cursor-brick mtl
    opt-env-conf path stm text typed-process vty vty-crossplatform
  ];
  executableHaskellDepends = [ base ];
  description = "Blind computing environment";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
  mainProgram = "echoic";
}
