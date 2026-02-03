final: prev:
with final.lib;
with final.haskell.lib;
{
  # Piper voice model (female - lessac)
  echoicPiperVoice =
    let
      voice = final.fetchurl {
        url = "https://huggingface.co/rhasspy/piper-voices/resolve/main/en/en_US/lessac/medium/en_US-lessac-medium.onnx";
        sha256 = "sha256-Xv4J5pkCGHgnr2RuGm6dJp3udp+Yd9F7FrG0buqvAZ8=";
      };
      voiceConfig = final.fetchurl {
        url = "https://huggingface.co/rhasspy/piper-voices/resolve/main/en/en_US/lessac/medium/en_US-lessac-medium.onnx.json";
        sha256 = "sha256-7+GcQXvtBV8taZCCSMa6ZQ+hNbyGiw5quz2hgdq2kKA=";
      };
    in
    final.runCommand "piper-voice-lessac" { } ''
      mkdir -p $out
      cp ${voice} $out/en_US-lessac-medium.onnx
      cp ${voiceConfig} $out/en_US-lessac-medium.onnx.json
    '';

  # Piper voice model (male - joe, neutral)
  echoicPiperVoiceMale =
    let
      voice = final.fetchurl {
        url = "https://huggingface.co/rhasspy/piper-voices/resolve/main/en/en_US/joe/medium/en_US-joe-medium.onnx";
        sha256 = "sha256-WK/OAyG42cRtfN+cFlAMxVp5O0IgIS26a3D7eIs7rwY=";
      };
      voiceConfig = final.fetchurl {
        url = "https://huggingface.co/rhasspy/piper-voices/resolve/main/en/en_US/joe/medium/en_US-joe-medium.onnx.json";
        sha256 = "sha256-PW1UELN5XLGVBZUkfvjwYZBxnm/b+jojVtjsNo4arTM=";
      };
    in
    final.runCommand "piper-voice-joe" { } ''
      mkdir -p $out
      cp ${voice} $out/en_US-joe-medium.onnx
      cp ${voiceConfig} $out/en_US-joe-medium.onnx.json
    '';

  echoicRelease =
    final.symlinkJoin {
      name = "echoic-release";
      paths = builtins.attrValues final.echoicReleasePackages;
      passthru = final.echoicReleasePackages;
    };

  echoicReleasePackages =
    mapAttrs
      (_: pkg: justStaticExecutables pkg)
      final.haskellPackages.echoicPackages;

  haskellPackages = prev.haskellPackages.override (old: {
    overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) (self: _super:
      let
        echoicPkg = name:
          buildFromSdist (overrideCabal
            (self.callPackage (../${name}) { })
            (old: {
              configureFlags = (old.configureFlags or [ ]) ++ [
                "--ghc-options=-O2"
                "--ghc-options=-Wall"
                "--ghc-options=-Wincomplete-uni-patterns"
                "--ghc-options=-Wincomplete-record-updates"
                "--ghc-options=-Wpartial-fields"
                "--ghc-options=-Widentities"
                "--ghc-options=-Wredundant-constraints"
                "--ghc-options=-Wcpp-undef"
                "--ghc-options=-Wunused-packages"
                "--ghc-options=-Werror"
              ];
              doHaddock = false;
              doCoverage = false;
              doHoogle = false;
              doCheck = false;
              hyperlinkSource = false;
              enableLibraryProfiling = false;
              enableExecutableProfiling = false;
            }));

        echoicPackages = {
          echoic = echoicPkg "echoic";
        };
      in
      {
        inherit echoicPackages;
      } // echoicPackages
    );
  });
}
