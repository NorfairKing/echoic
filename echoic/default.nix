{ mkDerivation, base, bytestring, lib, text, typed-process }:
mkDerivation {
  pname = "echoic";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base bytestring text typed-process ];
  description = "Blind computing environment";
  license = lib.licenses.mit;
  mainProgram = "echoic";
}
