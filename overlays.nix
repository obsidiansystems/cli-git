let
  cli-git = pkgs: ghc: with pkgs.haskell.lib; overrideCabal (ghc.callCabal2nix "cli-git" ./. {}) {
    librarySystemDepends = with pkgs; [
      git
    ];
  };
in {
  inherit cli-git;
}
