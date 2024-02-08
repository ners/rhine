{
  description = "rhine";
  nixConfig.bash-prompt = "\[rhine\]$ ";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable-small";
    nix-filter.url = "github:numtide/nix-filter";
    dunai = {
      url = "github:ivanperez-keera/dunai";
      flake = false;
    };
    flake-compat = {
      url = "github:nix-community/flake-compat";
      flake = false;
    };
  };

  outputs = inputs:
    with builtins;
    let
      inherit (inputs.nixpkgs) lib;
      foreach = xs: f: with lib; foldr recursiveUpdate { } (
        if isList xs then map f xs
        else if isAttrs xs then mapAttrsToList f xs
        else throw "foreach: expected list or attrset but got ${typeOf xs}"
      );
      hsSrc = root: inputs.nix-filter {
        inherit root;
        include = with inputs.nix-filter.lib; [
          "LICENSE"
          (matchExt "cabal")
          (matchExt "hs")
          (matchExt "md")
          isDirectory
        ];
      };
      pnames = [
        "rhine"
        "rhine-bayes"
        "rhine-examples"
        "rhine-gloss"
        "rhine-terminal"
      ];
      ghcs = [ "ghc92" "ghc94" ];
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = lib.composeExtensions
            prev.haskell.packageOverrides
            (hfinal: hprev:
              {
                dunai = hfinal.callCabal2nix "dunai" "${inputs.dunai}/dunai" { };
                bearriver = hfinal.callCabal2nix "bearriver" "${inputs.dunai}/dunai-frp-bearriver" { };
              }
              // foreach pnames (pname: {
                ${pname} = hfinal.callCabal2nix pname (hsSrc ./${pname}) { };
              })
            );
        };
      };
    in
    foreach inputs.nixpkgs.legacyPackages
      (system: pkgs':
        let
          pkgs = pkgs'.extend overlay;
          hps =
            lib.filterAttrs (ghc: _: elem ghc ghcs) pkgs.haskell.packages
            // { default = pkgs.haskellPackages; };
          mkPackage = pname:
            let
              inherit (hps.default.${pname}) name;
              allPackages = pkgs.linkFarm name (foreach hps (ghc: hp: {
                ${ghc} = hp.${pname};
              }));
              docs = pkgs.haskell.lib.documentationTarball hps.default.${pname};
              sdist = hps.default.cabalSdist {
                name = "${pname}.tar.gz";
                src = hsSrc ./${pname};
              };
            in
            pkgs.runCommand name { } (''
              mkdir $out
              cd $out
              ln -s ${allPackages} ${name}
            ''
            + lib.optionalString (pname != "rhine-examples") ''
              mkdir docs sdist
              ln -s ${docs}/*.tar.gz docs/
              ln -s ${sdist} sdist/${name}.tar.gz
            '');
          default = pkgs.symlinkJoin {
            name = "rhine-all";
            paths = map mkPackage pnames;
          };
        in
        {
          formatter.${system} = pkgs.nixpkgs-fmt;
          legacyPackages.${system} = { inherit (pkgs) haskell haskellPackages; };
          packages.${system} = { inherit default; };
          devShells.${system} =
            foreach hps (ghcName: hp: {
              ${ghcName} = hp.shellFor {
                packages = ps: map (pname: ps.${pname}) pnames;
                nativeBuildInputs = [
                  hps.default.cabal-install
                  hps.default.fourmolu
                  hp.haskell-language-server
                ];
              };
            });
        }
      ) // {
      overlays.default = overlay;
    };
}
