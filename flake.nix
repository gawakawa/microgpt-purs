{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    purs-nix.url = "github:purs-nix/purs-nix";
    ps-tools.follows = "purs-nix/ps-tools";
    flake-parts.url = "github:hercules-ci/flake-parts";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    git-hooks-nix = {
      url = "github:cachix/git-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
      ];

      imports = [
        inputs.treefmt-nix.flakeModule
        inputs.git-hooks-nix.flakeModule
      ];

      perSystem =
        {
          config,
          system,
          ...
        }:
        let
          pkgs = import inputs.nixpkgs { inherit system; };

          purs-nix = inputs.purs-nix { inherit system; };

          ps-tools = inputs.ps-tools.legacyPackages.${system};

          ps = purs-nix.purs {
            dependencies = [
              "console"
              "effect"
              "prelude"
            ];

            test-dependencies = [
              "test-unit"
            ];

            dir = ./.;
          };

        in
        {
          packages = with ps; {
            default = app { name = "hello"; };
            bundle = bundle { };
            output = output { };
          };

          apps.default = {
            type = "app";
            program = "${config.packages.default}/bin/hello";
          };

          pre-commit.settings.hooks = {
            treefmt = {
              enable = true;
              excludes = [ ".*\\.purs$" ]; # purs-tidy の mtime 問題を回避
            };
            statix.enable = true;
            deadnix.enable = true;
            actionlint.enable = true;
          };

          checks = {
            tests = ps.test.check { };
          };

          devShells.default = pkgs.mkShell {
            buildInputs = [
              pkgs.nodejs
              (ps.command { })
              purs-nix.esbuild
              purs-nix.purescript
            ]
            ++ config.pre-commit.settings.enabledPackages;
            shellHook = ''
              ${config.pre-commit.shellHook}
            '';
          };

          treefmt = {
            programs = {
              nixfmt = {
                enable = true;
                includes = [ "*.nix" ];
              };
            };
            settings.formatter.purs-tidy = {
              command = ps-tools.for-0_15.purs-tidy;
              options = [ "format-in-place" ];
              includes = [ "*.purs" ];
            };
          };
        };
    };
}
