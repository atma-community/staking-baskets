{
  modifyDevShell = { flake, system, shellHook, buildInputs }:
    flake.devShells.${system}.default.overrideAttrs
      (old: {
        shellHook = old.shellHook + shellHook;
        nativeBuildInputs = old.nativeBuildInputs ++ buildInputs;
      });

  bundle-purescript-module = { project, pkgs, purs, moduleName, bundled-module-name }: afterBundling:
    pkgs.runCommand "${moduleName}-bundle"
      {

        buildInputs = [
          pkgs.nodejs
          project.nodeModules
          project.compiled
        ];
        nativeBuildInputs = [
          purs
          pkgs.easy-ps.spago
        ];
      }
      ''
        mkdir $out

        export HOME="$TMP"
        export NODE_PATH="${project.nodeModules}/lib/node_modules"
        export PATH="${project.nodeModules}/bin:$PATH"

        cp -r ${project.compiled}/* .
        chmod -R +rwx .

        mkdir -p ./dist

        spago bundle-app --no-install --no-build -m "${moduleName}" \
              --to ./dist/${bundled-module-name}.js

        cp ./package-lock.json $out
        cp ./package.json $out

        ${afterBundling}

        mv dist $out
      '';

  buildPursDependencies =
    { name
    , strictComp ? true
    , censorCodes ? [ "UserDefinedWarning" ]
    , spagoPkgs
    , purs
    , pkgs
    , ...
    }:
    pkgs.stdenv.mkDerivation {
      inherit name;
      buildInputs = [
      ];
      nativeBuildInputs = [
        spagoPkgs.installSpagoStyle
        pkgs.easy-ps.psa
        purs
        pkgs.easy-ps.spago
      ];
      # Make the derivation independent of the source files.
      # `src` is not needed
      unpackPhase = "true";
      buildPhase = ''
        install-spago-style
        psa ${pkgs.lib.optionalString strictComp "--strict" } \
        --censor-lib \
        --is-lib=.spago ".spago/*/*/src/**/*.purs" \
        --censor-codes=${builtins.concatStringsSep "," censorCodes} \
        -gsourcemaps,js
      '';
      installPhase = ''
        mkdir $out
        mv output $out/
      '';
    };

}
