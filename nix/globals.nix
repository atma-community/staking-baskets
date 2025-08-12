{ haskell-nix, ctl, nixpkgs }:

rec {
  runtimeConfig = {
    network = {
      name = "preview";
      magic = 9;
    };
  };

  supportedSystems = [
    "x86_64-linux"
    "x86_64-darwin"
    "aarch64-linux"
    "aarch64-darwin"
  ];

  # generate `pkgs` with CTL's overlays applied. This gives you access to
  # various additional packages. The versions are the same as those that CTL
  # uses.
  nixpkgsFor = system: import nixpkgs {
    inherit system;
    overlays = [
      haskell-nix.overlay
      ctl.overlays.purescript
      ctl.overlays.runtime
      ctl.overlays.spago
    ];
    inherit (haskell-nix) config;
  };

  formattingDeps = system: with nixpkgsFor system;
    [
      fd
      dhall
      easy-ps.purs-tidy
      nixpkgs-fmt
      nodePackages.prettier
      nodePackages.eslint
    ];

  devDeps = system: with nixpkgsFor system;
    [
      nil
      nix-direnv
      nix-tree
      nix-du
    ];

  locale =
    ''
      export LC_CTYPE=C.UTF-8
      export LC_ALL=C.UTF-8
      export LANG=C.UTF-8
      export IN_NIX_SHELL='pure'
    '';

}
