{
  description = "My Haskell project";

  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-2311";
    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    iohkNix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    haskell-cross.url = "github:sambnt/haskell-cross";
  };

  outputs = { self, nixpkgs, haskellNix, iohkNix, haskell-cross, ... } @ inputs:
    let
      inherit (nixpkgs) lib;

      supportedSystems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];
      defaultSystem = lib.head supportedSystems;
      forAllSystems = lib.genAttrs supportedSystems;

      overlays = [
        haskellNix.overlay
        iohkNix.overlays.utils
        haskell-cross.overlays.default
        (import ./build-tools.nix)
      ];

      pkgsFor = system: import nixpkgs {
        inherit system overlays;
        inherit (haskellNix) config;
      };

      projectFor = system: let
        pkgs = pkgsFor system;
      in
        import ./project.nix haskell-cross pkgs.haskell-nix;

    in
    {
      legacyPackages = forAllSystems pkgsFor;
      packages = forAllSystems (system: let
        project = projectFor system;
      in {
        rort = project.hsPkgs.rort.components.exes.rort;
        default = self.packages.${system}.rort;

        rort-win64 = project.projectCross.mingwW64.hsPkgs.rort.components.exes.rort;
      });

      devShells = forAllSystems (system: let
        project = projectFor system;
      in {
        default = project.shell;
      });

      hydraJobs = let
        # Assume our CI machine is running Linux.
        pkgs = pkgsFor "x86_64-linux";
      in {
        build = forAllSystems (system: self.packages.${system}.default);

        dist-win64 = pkgs.zipDerivation self.packages.x86_64-linux.rort-win64;
        dist-linux64 = pkgs.mkLinuxPackage self.packages.x86_64-linux.default "rort";

        release = pkgs.releaseTools.aggregate
          { name = "rort-${self.packages.x86_64-linux.default.version}";
            constituents =
              [ self.hydraJobs.build.x86_64-linux
                self.hydraJobs.build.aarch64-linux
                self.hydraJobs.dist-win64
                self.hydraJobs.dist-linux64
                # Note: Your CI machine needs to be able to forward these builds
                # to a MacOS builder (see: https://nixos.org/manual/nix/stable/advanced-topics/distributed-builds).
                self.hydraJobs.build.x86_64-darwin
                self.hydraJobs.build.aarch64-darwin
              ];
            meta.description = "Release-critical builds";
          };
      };

      # Add the "haskell.nix" binary cache to your nix config.
      nixConfig = {
        extra-substituters = [
          "https://cache.iog.io"
        ];
        extra-trusted-public-keys = [
          "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
        ];
      };
    };
}
