{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
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
          # A few of these are because Alive LSP needs them.
          sbcl' = pkgs.sbcl.withPackages (ps: with ps; [
            alexandria
            bordeaux-threads
            caveman2
            cl-json
            cl-ppcre
            cl-yesql
            clack
            com_dot_inuoe_dot_jzon
            dexador
            djula
            envy
            flexi-streams
            fset
            iterate
            local-time
            make-hash
            parse-number
            postmodern
            rove
            str
            usocket
            uuid
            woo
          ]);
          tailwind =
            pkgs.nodePackages.tailwindcss.overrideAttrs (oa: {
              plugins = [
                pkgs.nodePackages."@tailwindcss/aspect-ratio"
                pkgs.nodePackages."@tailwindcss/forms"
                pkgs.nodePackages."@tailwindcss/language-server"
                pkgs.nodePackages."@tailwindcss/line-clamp"
                pkgs.nodePackages."@tailwindcss/typography"
              ];
            });
        in
        rec {
          # `nix develop`
          devShells.default = pkgs.mkShell {
            buildInputs = with pkgs; [
              nixpkgs-fmt
              rlwrap
              sbcl'
              tailwind
              tree
            ];
            shellHook = ''
              export DIRENV_LOG_FORMAT=""
              export CL_SOURCE_REGISTRY="$PWD/..//"
              export PATH=$(pwd)/bin:$PATH
            '';
          };

          checks = {
            unit-tests = pkgs.stdenv.mkDerivation {
              name = "unit-tests";
              src = ./.;
              doCheck = true;
              buildInputs = [ sbcl' ];
              checkPhase = ''
                mkdir -p $out/tmp
                export HOME=$out/tmp
                export CL_SOURCE_REGISTRY="$src//";

                ${sbcl'}/bin/sbcl --eval "(load (sb-ext:posix-getenv \"ASDF\"))" --eval "(asdf:test-system :cave)" --non-interactive
              '';
            };
          };
        });
}
