{
  perSystem = { config, pkgs, ... }: {
    # Default shell.
    devShells.default = pkgs.mkShell {
      name = "yesblog";
      meta.description = "Haskell development environment";
      # See https://community.flake.parts/haskell-flake/devshell#composing-devshells
      inputsFrom = [
        config.haskellProjects.default.outputs.devShell # See ./nix/modules/haskell.nix
        config.pre-commit.devShell # See ./nix/modules/formatter.nix
      ];
      packages = with pkgs; [
        just
        nixd
        ghciwatch
      ];

      venvDir = "./.venv";
      buildInputs = with pkgs; [
        # A Python interpreter including the 'venv' module is required to bootstrap
        # the environment.
        python3Packages.python

        # This executes some shell code to initialize a venv in $venvDir before
        # dropping into the shell
        python3Packages.venvShellHook

        # Those are dependencies that we would like to use from nixpkgs, which will
        # add them to PYTHONPATH and thus make them accessible from within the venv.
        python3Packages.numpy
      ];

      # Run this command, only after creating the virtual environment
      postVenvCreation = ''
        unset SOURCE_DATE_EPOCH
        pip install -r requirements.txt
      '';

      # Now we can execute any commands within the virtual environment.
      # This is optional and can be left out to run pip manually.
      postShellHook = ''
        # allow pip to install wheels
        unset SOURCE_DATE_EPOCH
      '';
    };
  };
}
