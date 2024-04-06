{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
  };

  outputs = { self, nixpkgs, nix-filter, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = import nixpkgs {
            inherit system;
          };
          lib = pkgs.lib;
          stdenv = pkgs.stdenv;
          sbcl' = pkgs.sbcl.withPackages (ps: with ps; [
            alexandria
            bordeaux-threads
            caveman2
            clack
            cl-dbi
            cl-json
            cl-ppcre
            datafly
            djula
            envy
            hunchentoot
            iterate
            local-time
            parse-number
            postmodern
            str
            woo
          ]);
          roswell' = pkgs.roswell.override { sbcl = sbcl'; };
        in
        rec {
          # `nix develop`
          devShells.default = pkgs.mkShell {
            buildInputs = with pkgs; [
              nixpkgs-fmt
              sbcl'
              roswell'
              tree
            ];
            shellHook = ''
              export DIRENV_LOG_FORMAT=""
              export CL_SOURCE_REGISTRY=$(pwd)//:
              export PATH=$(pwd)/bin:$PATH
            '';
          };
        });
}
