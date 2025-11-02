{
        description = "hephaestus";

        inputs = {
                nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
                rust-overlay.url = "github:oxalica/rust-overlay";
        };

        outputs = { self, nixpkgs, rust-overlay, ... } @ inputs: let
                t = "\t";
                script = {
                        flake-update = {
                                name = "flake-update";
                                code = ''
                                        bin/flake-update "$@"
                                '';
                        };
                        make = {
                                name = "make";
                                code = ''
                                        bin/make "$@"
                                '';
                        };
                };
                forAllSystems = flake: nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed (system: flake rec {
                        pkgs = import nixpkgs {
                                inherit system;
                                overlays = [ (import rust-overlay) ];
                        };
                        script-bin = {
                                flake-update = pkgs.writeShellScriptBin
                                        script.flake-update.name
                                        script.flake-update.code;
                                make = pkgs.writeShellScriptBin
                                        script.make.name
                                        script.make.code;
                        };
                });
        in {
                devShells = forAllSystems ({ pkgs, script-bin, ... }: rec {
                        default = with pkgs; mkShellNoCC {
                                packages = [
                                        (rust-bin.selectLatestNightlyWith (toolchain: toolchain.default.override {
                                                extensions = [ "rust-src" ];
                                        }))
                                        bashInteractive
                                        bc
                                        cargo-expand
                                        cargo-llvm-cov
                                        git
                                        grcov
                                        llvmPackages.llvm
                                        xidel

                                        script-bin.flake-update
                                        script-bin.make
                                ];
                                env = {
                                        inherit (pkgs.cargo-llvm-cov) LLVM_COV LLVM_PROFDATA;
                                };
                        };
                });
        };
}
