{ self, system, tooling, deps, shellHook, buildInputs, ... }:
let
  onchainFlake =
    tooling.lib.mkFlake { inherit self; }
      {
        imports = [
          (tooling.lib.mkHaskellFlakeModule1 {
            project.modules = [
              ({ config, ... }: {
                packages.plutus-simple-model.doHaddock = false;
              })
            ];
            project.src = ./../onchain;
            project.extraHackage = deps;

            project.shell = {
              withHoogle = true;
            };
          })
        ];

      };

  utils = import ./utils.nix;
  devShell = utils.modifyDevShell { flake = onchainFlake; inherit shellHook buildInputs system; };
in
{
  flake = onchainFlake;
  serialize-scripts = onchainFlake.packages.${system}."atrium:exe:serialise";
  nativeBuildInputs = onchainFlake.devShells.${system}.default.nativeBuildInputs;
  packages = onchainFlake.packages.${system};
  apps = onchainFlake.apps.${system};
  inherit devShell;
}
