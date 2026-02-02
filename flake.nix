{
  description = "echoic - blind computing environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    autodocodec.url = "github:NorfairKing/autodocodec";
    autodocodec.flake = false;
    cursor.url = "github:NorfairKing/cursor";
    cursor.flake = false;
    cursor-brick.url = "github:NorfairKing/cursor-brick";
    cursor-brick.flake = false;
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text";
    safe-coloured-text.flake = false;
    opt-env-conf.url = "github:NorfairKing/opt-env-conf";
    opt-env-conf.flake = false;
  };

  outputs =
    { self
    , nixpkgs
    , pre-commit-hooks
    , autodocodec
    , cursor
    , cursor-brick
    , safe-coloured-text
    , opt-env-conf
    }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          self.overlays.default
          (import (autodocodec + "/nix/overlay.nix"))
          (import (cursor + "/nix/overlay.nix"))
          (import (cursor-brick + "/nix/overlay.nix"))
          (import (safe-coloured-text + "/nix/overlay.nix"))
          (import (opt-env-conf + "/nix/overlay.nix"))
        ];
        config.allowUnfreePredicate = pkg: builtins.elem (pkgs.lib.getName pkg) [
          "echoic"
        ];
      };
    in
    {
      overlays.default = import ./nix/overlay.nix;

      packages.${system} = {
        default = pkgs.echoicRelease;
        echoic = pkgs.echoicReleasePackages.echoic;
      };

      checks.${system} = {
        release = self.packages.${system}.default;
        shell = self.devShells.${system}.default;
        pre-commit = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            ormolu.enable = true;
            hlint.enable = true;
            hpack.enable = true;
            nixpkgs-fmt.enable = true;
            nixpkgs-fmt.excludes = [ ".*/default.nix" ];
            cabal2nix.enable = true;
          };
        };
      };

      devShells.${system}.default = pkgs.haskellPackages.shellFor {
        name = "echoic-shell";
        packages = p: [ p.echoic ];
        withHoogle = true;
        buildInputs = with pkgs; [
          # Haskell tooling
          stack
          haskellPackages.haskell-language-server

          # TTS
          piper-tts

          # Audio playback
          alsa-utils
          sox
        ] ++ self.checks.${system}.pre-commit.enabledPackages;

        shellHook = ''
          ${self.checks.${system}.pre-commit.shellHook}
          export PIPER_VOICE="${pkgs.echoicPiperVoice}/en_US-lessac-medium.onnx"
          echo "Echoic development shell"
          echo "Voice model: $PIPER_VOICE"
        '';
      };
    };
}
