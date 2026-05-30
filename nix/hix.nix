{pkgs, ...}: {
  compiler-nix-name = "ghc98";

  crossPlatforms = p: pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [
    p.musl64
  ];

  shell.tools.cabal = "latest";
}
