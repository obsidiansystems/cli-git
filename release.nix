{ }:
let
  nixpkgsSets = import ./.ci/nixpkgs.nix;
  overlays = import ./overlays.nix;
  inherit (nixpkgsSets) nixos1809 nixos2003 unstable;
  inherit (nixos2003) lib;
  inherit (nixos2003.haskell.lib) doJailbreak dontCheck overrideCabal;
  commonOverrides = self: super: {
    which = self.callHackageDirect {
      pkg = "which";
      ver = "0.2";
      sha256 = "1g795yq36n7c6ycs7c0799c3cw78ad0cya6lj4x08m0xnfx98znn";
    } {};
    whichExecutables = p: with p; [
     git
    ];
    cli-extras = self.callCabal2nix "cli-extras" (import ./dep/cli-extras/thunk.nix) {};
  };
  ghcs = rec {
    ghc865 = nixos2003.haskell.packages.ghc865.override {
      overrides = commonOverrides;
    };
    ghc884 = nixos2003.haskell.packages.ghc884.override {
      overrides = commonOverrides;
    };
  };
in
  lib.mapAttrs (_: overlays.cli-git nixos2003) ghcs
