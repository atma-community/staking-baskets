{
  description = "atrium";
  nixConfig = {
    bash-prompt = "\\[\\e[0m\\][\\[\\e[0;2m\\]nix-develop \\[\\e[0;1m\\]atrium \\[\\e[0;93m\\]\\w\\[\\e[0m\\]]\\[\\e[0m\\]$ \\[\\e[0m\\]";
    allow-import-from-derivation = "true";
  };

  inputs = {
    ctl = {
      type = "github";
      owner = "Renegatto";
      repo = "cardano-transaction-lib";
      rev = "9849aa85db081defb32ea7d22567fdb9362d75e7";
    };
    # To use the same version of `nixpkgs` as we do
    nixpkgs.follows = "ctl/nixpkgs";
    nixpkgs-upstream.url = "github:nixos/nixpkgs/nixpkgs-unstable";


    tooling.url = "github:mlabs-haskell/mlabs-tooling.nix";
    haskell-nix.follows = "tooling/haskell-nix";

    # onchain inputs
    plutarch.url = "github:Plutonomicon/plutarch/01a67f56b2bf428538e92ed9ada0ce88d90ab636";
    ply.url = "github:mlabs-haskell/ply/2cda3b44f87c659980bea2bc0b4a822d1e9eaef4";
    liqwid-libs.url = "github:liqwid-Labs/liqwid-libs/f747c0b50e4035d4f3a13f4f3c028855466a3469";
    plutus-simple-model.url = "github:mlabs-haskell/plutus-simple-model/2e4240703fedd86152fedb97b7664278c97d64a1";

    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "ctl/nixpkgs";
    };

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    hercules-ci-effects.url = "github:hercules-ci/hercules-ci-effects";

    # For deployment
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
  };

  outputs =
    { self
    , nixpkgs
    , nixpkgs-upstream
    , haskell-nix
    , pre-commit-hooks
    , ctl
    , tooling
    , plutarch
    , ply
    , liqwid-libs
    , plutus-simple-model
    , hercules-ci-effects
    , ...
    }:
    let

      globals = import ./nix/globals.nix { inherit haskell-nix ctl nixpkgs; };
      utils = import ./nix/utils.nix;
      nixpkgsFor = globals.nixpkgsFor;

      perSystem = nixpkgs.lib.genAttrs globals.supportedSystems;
      plainNixpkgsFor = system: import nixpkgs-upstream { inherit system; };


      # This adds
      # - a shellHook (pre-commit-check.shellHook) attribute you can just add to
      #   your shell with the // operator if you add the shellHook, in your
      #   shell a new command becomes available "`pre-commit`" which you can run
      #   by hand with `pre-commit run --all` and gets run automatically before
      #   each commit
      #
      # - a check (pre-commit-check) that will check formatting according to
      # - what you specified
      #
      # - for further tooling this makes available please refer to
      # - https://github.com/cachix/pre-commit-hooks.nix
      #   (it is also quite easy to add tools by yourself)
      preCommitCheckFor = system:
        pre-commit-hooks.lib.${system}.run
          {
            src = ./.;

            settings = { };
            tools = { };

            hooks = {
              nixpkgs-fmt.enable = true;

              purescript-format = {
                enable = true;

                # The name of the hook (appears on the report table):
                name = "purescript format";

                # The command to execute (mandatory):
                entry = "make -f offchain/Makefile format";

                # The pattern of files to run on (default: "" (all))
                # see also https://pre-commit.com/#hooks-files
                files = "";

                # List of file types to run on (default: [ "file" ] (all files))
                # see also https://pre-commit.com/#filtering-files-with-types
                # You probably only need to specify one of `files` or `types`:
                types = [ "file" ];

                # Exclude files that were matched by these patterns
                # (default: [] (none)):
                excludes = [ ];

                # The language of the hook - tells pre-commit
                # how to install the hook (default: "system")
                # see also https://pre-commit.com/#supported-languages
                language = "system";

                # Set this to false to not pass the changed files
                # to the command (default: true):
                pass_filenames = false;
              };

              haskell-format = {
                enable = true;

                # The name of the hook (appears on the report table):
                name = "haskell format";

                # The command to execute (mandatory):
                entry = "make -f onchain/Makefile format";

                # The pattern of files to run on (default: "" (all))
                # see also https://pre-commit.com/#hooks-files
                files = "";

                # List of file types to run on (default: [ "file" ] (all files))
                # see also https://pre-commit.com/#filtering-files-with-types
                # You probably only need to specify one of `files` or `types`:
                types = [ "file" ];

                # Exclude files that were matched by these patterns
                # (default: [] (none)):
                excludes = [ ];

                # The language of the hook - tells pre-commit
                # how to install the hook (default: "system")
                # see also https://pre-commit.com/#supported-languages
                language = "system";

                # Set this to false to not pass the changed files
                # to the command (default: true):
                pass_filenames = false;
              };
            };
          };

      onchainTooling = system:
        let
          deps = [
            "${plutarch}"
            "${plutarch}/plutarch-extra"
            "${plutarch}/plutarch-test"
            "${ply}/ply-core"
            "${ply}/ply-plutarch"
            "${liqwid-libs}/plutarch-unit"
            "${liqwid-libs}/plutarch-context-builder"
            "${liqwid-libs}/plutarch-quickcheck"
            "${liqwid-libs}/liqwid-plutarch-extra"
            "${plutus-simple-model}/psm"
            "${plutus-simple-model}/cardano-simple"
          ];
        in
        import ./nix/onchain.nix {
          inherit self system tooling deps;
          shellHook = (preCommitCheckFor system).shellHook;
          buildInputs = globals.formattingDeps system;
        };

      infraFlake = tooling.lib.mkFlake { inherit self; }
        {
          imports = [ ./nix/infra ];
          systems = globals.supportedSystems;
        };

      herculesFlake = tooling.lib.mkFlake { inherit self; } ({ withSystem, config, ... }: {
        imports = [
          hercules-ci-effects.flakeModule
        ];

        systems = [ "x86_64-linux" ];

        hercules-ci.github-releases = {
          files = withSystem config.defaultEffectSystem ({ self', pkgs, ... }:
            [
              {
                label = "atrium-release.zip";
                paths = [ "${self'.packages.bundled-release}/artifacts" ];
                archiver = "zip";
              }
            ]);
        };

        herculesCI = herculesCI@{ ... }:
          let
            mkGithubPagesDeploy = withSystem config.defaultEffectSystem ({ pkgs, hci-effects, self', ... }:
              let
                sourceBranch = "main";
                settings = {
                  git.checkout.remote.url = herculesCI.config.repo.remoteHttpUrl;
                  git.checkout.forgeType = "github";
                  git.checkout.user = "x-access-token";
                  git.update.branch = "gh-pages";
                  secretsMap.token = { type = "GitToken"; };
                };
              in
              hci-effects.runIf (herculesCI.config.repo.branch == sourceBranch) (
                hci-effects.gitWriteBranch {
                  imports = [
                    settings
                  ];
                  contents = "${self'.packages.bundled-release}/artifacts/client-side/dist";
                }));
          in
          {
            onPush.github-pages-deploy.outputs.effects.default = mkGithubPagesDeploy;
            ciSystems = [ "x86_64-linux" ];
          };
      });




      scripts-bundle = system:
        let
          pkgs = nixpkgsFor system;
          # unlines: Array String -> String
          unlines = pkgs.lib.strings.concatMapStrings (s: s + "\n");
          # dirHash: DirectoryPath -> Md5Hash 
          dirHash = dirPath: filesHash (pkgs.lib.filesystem.listFilesRecursive dirPath);
          # filesHash: [FilePath] -> Md5Hash
          filesHash = filenames:
            builtins.hashString "md5"
              (pkgs.lib.strings.concatMapStrings
                (builtins.hashFile "md5")
                filenames);
          # make-bare-scripts: Command
          make-bare-scripts = pkgs.runCommand
            "serialise-scripts"
            {
              nativeBuildInputs = [ (onchainTooling system).serialize-scripts ];
            }
            ''
              mkdir $out
              sh ${./offchain/serialise-scripts.sh} $out
            '';
          # make-hashed-scripts: Command
          make-hashed-scripts = pkgs.runCommand "make-hashed-scripts" { }
            ''
              mkdir $out
              ${hash-files {dir = make-bare-scripts; outDir = "$out";}}
            '';
          # fileName: FilePath -> FileName | error
          fileName = path: builtins.head (builtins.match ".*/([^/]*)" path);
          # prefix-file-with-hash: {path: Path, outDir: DirPath} -> BashScript
          prefix-file-with-hash = { path, outDir }:
            let fileHash = builtins.hashFile "md5" path;
            in "cp ${path} ${outDir}/${fileHash}-${fileName path}";
          # hash-files: Path -> BashScript
          hash-files = { dir, outDir }:
            let
              unhashed = pkgs.lib.filesystem.listFilesRecursive dir;
              copyHashed = path: prefix-file-with-hash { inherit path; inherit outDir; };
            in
            unlines (builtins.map copyHashed unhashed);

          scripts-dir = "$out/${dirHash make-hashed-scripts}-scripts";
        in
        pkgs.runCommand "scripts-bundle" { }
          ''
            mkdir $out
            mkdir ${scripts-dir}
            cp -r ${make-hashed-scripts}/* ${scripts-dir}
          '';

    in
    {

      # NOTE: this is convenient for use in nix repl
      inherit plainNixpkgsFor;

      packages = perSystem (system:
        let
          # workaround for https://github.com/mlabs-haskell/mlabs-tooling.nix/issues/15
          withoutHaddock = builtins.removeAttrs (onchainTooling system).flake.packages.${system} [ "haddock" ];
        in
        withoutHaddock //
        {

          ctl-runtime = (nixpkgsFor system).buildCtlRuntime globals.runtimeConfig;

          # a possibility to generate release locally, for debugging purposes
          release = release-bundle system;

          # generate local nodejs executables
          atrium-reward-backend-purescript = atriumRewardsBackendPurescript system;
          atrium-snapshot-bot = snapshotBotFor system;

          # generate independent executables and js bundles
          bundled-db-server = db-server-bundle system;
          bundled-release = release-bundle system;
          bundled-basket-info-app = basket-info-app-bundle system;
          bundled-basket-bot = basket-bot-bundle system;
          bundled-demo-ui = demo-ui-bundle system;
          bundled-web-ui = web-ui-bundle system;
          bundled-rewards-backend = rewards-backend-bundle system;
          bundled-snapshot-bot = snapshot-bot-bundle system;
          bundled-scripts = scripts-bundle system;
          bundled-db-server-openapi = db-server-openapi system;

          deploy = infraFlake.packages.${system}.deploy;
        }
      );

      apps = perSystem (system: {
        inherit ((onchainTooling system).flake.apps.${system}) format;
        # docs = (offchainTooling system).launchSearchablePursDocs { };
        deploy = infraFlake.apps.${system}.deploy;
        ctl-runtime = (nixpkgsFor system).launchCtlRuntime globals.runtimeConfig;
      });

      # This is used for nix build .#check.<system> because nix flake check
      # does not work with haskell.nix import-from-derivtion.
      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-check"
          {
            nativeBuildInputs = builtins.attrValues self.checks.${system}
            ++ self.devShells.${system}.onchain.nativeBuildInputs
          } "touch $out");

      checks = perSystem (system:
        let
          pkgs = nixpkgsFor system;
          project = offchainTooling system;
          nodejs = pkgs.nodejs;
          testMain = "Test.Main";
        in
        (onchainTooling system).flake.checks.${system}
        // {
          ctlPsFormatChecks = ctlPsFormatChecksFor system;
          ctlJsFormatChecks = ctlJsFormatChecksFor system;
          atrium-ctl = pkgs.runCommand "atrium-ctl"
            {
              buildInputs = with pkgs ; [
                project.nodeModules
                ogmios
                plutip-server
                kupo
                (atriumRewardsTooling system).db-server
                pkgs.postgresql
              ];
              src = ./haskell-backend;
              NODE_PATH = "${project.nodeModules}/lib/node_modules";
            }
            ''
              mkdir $out && cd $out

              mkdir haskell-backend
              cp -r ${./haskell-backend}/* ./haskell-backend/

              mkdir configs
              cp -r ${./configs}/* ./configs/

              mkdir offchain && cd offchain
              cp -r ${project.compiled}/* .

              ${nodejs}/bin/node --enable-source-maps -e 'require("./output/${testMain}").main()'
            '';
          #
        }
      );

      devShells = perSystem (system: {
        onchain = (onchainTooling system).devShell;
        infra = infraFlake.devShells.${system}.default;
        offchain = (offchainTooling system).devShell;
        atrium-rewards = (atriumRewardsTooling system).devShell;
      });

      hydraJobs = {
        checks = { inherit (self.checks) x86_64-linux; };
        packages = { inherit (self.packages) x86_64-linux; };
        devShells = { inherit (self.devShells) x86_64-linux; };
      };
      nixosConfigurations = infraFlake.nixosConfigurations;
      nixosModules = infraFlake.nixosModules;
      hercules-ci = perSystem (system: {
        github-pages.settings = {
          contents = "${self.packages.${system}.bundled-release}/artifacts/dist";
        };
      });
    } // herculesFlake;
}
