{
  description = "...";

  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  };

  inputs = {
    # IMPORTANT: report any change to nixpkgs channel in nix/default.nix:
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    hostNixpkgs.follows = "nixpkgs";
    hackageNix = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };
    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.hackage.follows = "hackageNix";
    };
    CHaP = {
      url = "github:intersectmbo/cardano-haskell-packages?ref=repo";
      flake = false;
    };
    utils.url = "github:numtide/flake-utils";

    # For cabalWrapped:
    iohkNix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs: let
    supportedSystems = [
      "x86_64-linux"
      #"x86_64-darwin"
      #"aarch64-linux"
      #"aarch64-darwin"
    ];
  in
    inputs.utils.lib.eachSystem supportedSystems (
      system: let
        # setup our nixpkgs with the haskell.nix overlays:
        nixpkgs = import inputs.nixpkgs {
          overlays =
            [inputs.haskellNix.overlay]
            ++ # For cabalWrapped:
            builtins.attrValues inputs.iohkNix.overlays;
          inherit system;
          inherit (inputs.haskellNix) config;
        };
        inherit (nixpkgs) lib;
        inherit (nixpkgs.haskell-nix) haskellLib;

        # We use cabalProject' to ensure we don't build the plan for
        # all systems.
        cabalProject = nixpkgs.haskell-nix.cabalProject' {
          src = ./.;
          name = "hasdiff";
          compiler-nix-name = "ghc9101";

          # CHaP input map, so we can find CHaP packages (needs to be more
          # recent than the index-state we set!). Can be updated with
          #
          #  nix flake lock --update-input CHaP
          #
          inputMap = {
            "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.CHaP;
          };

          shell.packages = ps: with ps; [
            hasdiff
          ];

          # tools we want in our shell
          # TODO: removed tools to get building properly
          shell.tools = {
            # cabal = "3.10.1.0";
            #ghcid = "0.8.8";
            #haskell-language-server = "latest";
            #hlint = {};
            #fourmolu = "0.10.1.0";
          };
          # Now we use pkgsBuildBuild, to make sure that even in the cross
          # compilation setting, we don't run into issues where we pick tools
          # for the target.
          shell.nativeBuildInputs = with nixpkgs.pkgsBuildBuild; [
          #  (pkgs.python3.withPackages (ps: with ps; [sphinx sphinx_rtd_theme recommonmark sphinx-markdown-tables sphinxemoji]))
            #  haskellPackages.implicit-hie
            cabalWrapped
          ];
          shell.withHoogle = true;

          # package customizations as needed. Where cabal.project is not
          # specific enough, or doesn't allow setting these.
          modules = [
            ({pkgs, ...}: {
              # packages.PACKAGE_NAME = {
              #   configureFlags = [
              #     "--ghc-option=-Werror"
              #   ];
              #   components = {
              #   };
              # };
            })
            ({pkgs, ...}:
              lib.mkIf pkgs.stdenv.hostPlatform.isUnix {
              })
          ];
        };
        # ... and construct a flake from the cabal project
        flake =
          cabalProject.flake (
            # we also want cross compilation to windows.
            lib.optionalAttrs (system == "x86_64-linux") {
              #crossPlatforms = p: [p.mingwW64];
            }
          );
      in
        nixpkgs.lib.recursiveUpdate flake rec {
          legacyPackages = rec {
            inherit cabalProject;
          };
          # `nix develop .#profiling`: a shell with profiling enabled
          devShells.profiling =
            (cabalProject.appendModule
              { modules =
                  [ { enableLibraryProfiling = true;
                    }
                  ];
              }).shell;
          # formatter used by nix fmt
          formatter = nixpkgs.alejandra;
        }
    );
}
