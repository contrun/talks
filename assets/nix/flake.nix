{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    # Waiting for https://github.com/edolstra/flake-compat/pull/26
    flake-compat-result = {
      url = "github:teto/flake-compat/8e15c6e3c0f15d0687a2ab6ae92cc7fab896bfed";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let pkgs = nixpkgs.legacyPackages.${system}; in
        rec {
          packages = flake-utils.lib.flattenTree {
            hello = pkgs.hello;
            gitAndTools = pkgs.gitAndTools;
            emacs = pkgs.emacs;
            emacsNativeComp = pkgs.emacs.override {
              nativeComp = true;
            };
          };
          defaultPackage = packages.hello;

          apps.hello = flake-utils.lib.mkApp { drv = packages.hello; };
          defaultApp = apps.hello;

          devShells = {
            rust = pkgs.mkShell {
              buildInputs = with pkgs; [ rustup rust-analyzer ];
            };
            go = pkgs.mkShell {
              buildInputs = with pkgs; [ go gopls ];
            };
          };
          devShell = devShells.rust;

          legacyPackages = pkgs;
        }
      );
}
