{
        description = "hephaestus";

        inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
        inputs.flake-utils.url = "github:numtide/flake-utils";

        outputs = { self, nixpkgs, flake-utils }:

        flake-utils.lib.eachDefaultSystem (system:
                let
                        pkgs  = nixpkgs.legacyPackages.${system};
                        hPkgs = pkgs.haskell.packages."ghc9103"; # This must match what is in stackage.yaml
                        haskellDevTools = [
                                hPkgs.ghc                     # GHC compiler in the desired version (will be available on PATH)
                                hPkgs.ghcid                   # Continuous terminal Haskell compile checker
                                hPkgs.fourmolu                # Haskell formatter
                                hPkgs.hlint                   # Haskell codestyle checker
                                hPkgs.hoogle                  # Lookup Haskell documentation
                                hPkgs.haskell-language-server # LSP server for editor
                                hPkgs.implicit-hie            # auto generate LSP hie.yaml file from cabal
                                # hPkgs.retrie                  # Haskell refactoring tool
                                # hPkgs.cabal-install
                                stack-wrapped
                                pkgs.bashInteractive
                                pkgs.git
                                pkgs.zlib                     # External C library needed by some Haskell packages
                        ];
                        stack-wrapped = pkgs.symlinkJoin {
                                name = "stack"; # will be available as the usual `stack` in terminal
                                paths = [ pkgs.stack ];
                                buildInputs = [ pkgs.makeWrapper ];
                                postBuild = ''
                                        wrapProgram $out/bin/stack \
                                            --add-flags "\
                                                --no-nix \
                                                --system-ghc \
                                                --no-install-ghc \
                                            "
                                '';
                        };
                in {
                        devShells.default = pkgs.mkShell {
                                buildInputs = haskellDevTools;
                                LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath haskellDevTools;
                                LOCALE_ARCHIVE_2_11 = "${pkgs.glibcLocales}/lib/locale/locale-archive";
                                LOCALE_ARCHIVE_2_27 = "${pkgs.glibcLocales}/lib/locale/locale-archive";
                        };
                }
        );
}
