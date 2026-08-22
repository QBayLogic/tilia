{
  description = "A formatter for Haskell source code";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    # Stackage is not used here; pointing it at an empty flake stops
    # nix-direnv from downloading the snapshot on every shell entry.
    haskellNix.inputs.stackage.follows = "emptyFlake";
    emptyFlake.url = "github:input-output-hk/empty-flake";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, haskellNix, flake-utils, ... }:
    let
      inherit (nixpkgs) lib;

      compilers = [ "ghc9103" "ghc9124" "ghc9141" ];
      baseCompiler = builtins.head compilers;

      # Files that participate in the build. Anything outside this set can
      # change without forcing a rebuild.
      sourceDirs = [ "src" "app" "tests" ];
      sourceFiles = [ "cabal.project" "tilia.cabal" ];

      # Cabal insists these exist, but their contents never affect the
      # build, so they are staged as empty placeholders.
      placeholders = [ "LICENSE.md" "CHANGELOG.md" "README.md" ];

      perCompiler = prefix: f:
        lib.listToAttrs
          (map (c: lib.nameValuePair "${prefix}${c}" (f c)) compilers);
    in
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          inherit (haskellNix) config;
          overlays = [ haskellNix.overlay ];
        };

        src =
          let
            root = toString ./.;
            wanted = path:
              let rel = lib.removePrefix "${root}/" (toString path); in
              lib.elem rel sourceFiles
              || lib.any (d: rel == d || lib.hasPrefix "${d}/" rel) sourceDirs;
            pruned = pkgs.haskell-nix.haskellLib.cleanSourceWith {
              name = "tilia-source";
              src = ./.;
              filter = path: _type: wanted path;
            };
          in
          pkgs.runCommand "tilia-source-staged" { } ''
            cp -r ${pruned} $out
            chmod -R u+w $out
            touch ${lib.concatMapStringsSep " " (f: "$out/${f}") placeholders}
          '';

        projects = lib.genAttrs compilers (compiler:
          pkgs.haskell-nix.cabalProject {
            inherit src;
            compiler-nix-name = compiler;
            modules = [{ packages.tilia.writeHieFiles = true; }];
          });

        exeFor = compiler: projects.${compiler}.tilia.components.exes.tilia;
        testsFor = compiler: projects.${compiler}.tilia.checks.tests;

        # Weeder needs one --hie-directory per component it should see.
        weeder =
          let
            project = projects.${baseCompiler};
            inherit (project.tilia.components) library exes tests;
            scanned = [ library exes.tilia tests.tests ];
          in
          pkgs.runCommand "tilia-weeder"
            { nativeBuildInputs = [ (project.tool "weeder" "2.10.0") ]; }
            ''
              weeder --config ${./weeder.toml} \
                ${lib.concatMapStringsSep " \\\n    "
                    (c: "--hie-directory ${c.hie}") scanned}
              touch $out
            '';
      in
      {
        packages =
          { default = exeFor baseCompiler; }
          // perCompiler "tilia-" exeFor;

        checks =
          { inherit weeder; }
          // perCompiler "tests-" testsFor;

        apps.default = {
          type = "app";
          program = "${exeFor baseCompiler}/bin/tilia";
        };

        devShells.default = projects.${baseCompiler}.shellFor {
          tools.cabal = "latest";
          withHoogle = false;
          exactDeps = false;
        };
      });

  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
  };
}
