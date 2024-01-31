######################################################################
pkgs: super: let
  cabalProjectIndexState = cabalProject: let
    parsed = import (pkgs.runCommandNoCC "cabal-project-index-state.nix" {
      preferLocalBuild = true;
      allowSubstitutes = false;
    } ''
      awk 'BEGIN { print "{"; } END { print "}"; } /^(index-state|with-compiler)/ { gsub(/:/, "", $1); print"  " $1 " = \"" $2 "\";"; }' < ${cabalProject} > $out
    '');
  in {
    index-state = parsed.index-state or null;
    compiler-nix-name = if parsed ? with-compiler
      then pkgs.lib.replaceStrings ["-" "."] ["" ""] parsed.with-compiler
      else null;
  };

  tools = {
    fourmolu = {
      version                = "0.11.0.0";
    };
    # hie-bios.version = "0.13.0";
    hlint.version                   = "3.6.1";
    lentil.version                  = "1.5.6.0";
    stylish-haskell.version         = "0.14.5.0";
    # weeder.version                  = "2.7.0";
    hoogle = {
       version                  = "5.0.18.3";
       cabalProjectLocal = ''
       constraints: warp == 3.3.25,
                    warp-tls == 3.3.6,
                    tls == 1.6.0,
                    http2 == 4.1.4
       '';
    };
    haskell-language-server = {
      version = "2.3.0.0";
      modules = [{ reinstallableLibGhc = false; }];
      cabalProjectLocal = ''
      allow-newer:
        *:lsp-test
      '';
    };
  };

  # Use cabal.project as the source of GHC version and Hackage index-state.
  inherit (cabalProjectIndexState ./cabal.project)
    index-state compiler-nix-name;

  hsPkgs = pkgs.lib.mapAttrs mkTool tools;

  mkTool = name: args: pkgs.haskell-nix.hackage-package ({
    inherit name index-state compiler-nix-name;
  } // builtins.removeAttrs args ["exe"]);

  # Get the actual tool executables from the haskell packages.
  mapExes = pkgs.lib.mapAttrs (name: hsPkg: hsPkg.components.exes.${tools.${name}.exe or name});

in {
  # inherit index-state compiler-nix-name;

  haskell-build-tools = pkgs.recurseIntoAttrs
    ((super.haskell-build-tools or {})
      // mapExes hsPkgs
      // {
        haskell-language-server-wrapper = pkgs.runCommandNoCC "haskell-language-server-wrapper" {} ''
          mkdir -p $out/bin
          hls=${hsPkgs.haskell-language-server.components.exes.haskell-language-server}
          ln -s $hls/bin/haskell-language-server $out/bin/haskell-language-server-wrapper
        '';
      });
}
