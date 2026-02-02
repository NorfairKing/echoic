{ mkDerivation, base, bytestring, lib, opt-env-conf, path, text
, typed-process
}:
mkDerivation {
  pname = "echoic";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring opt-env-conf path text typed-process
  ];
  executableHaskellDepends = [ base ];
  description = "Blind computing environment";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
  mainProgram = "echoic";
}
