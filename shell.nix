let
  sources = import nix/sources.nix;
  pkgs = import sources.nixpkgs { };
  nix-pre-commit-hooks = import sources."pre-commit-hooks.nix";
  pre-commit-check = nix-pre-commit-hooks.run {
    src = ./.;
    hooks = {
      elm-format = {
        enable = true;
      };
      prettier = {
        excludes = [
          "review/suppressed/"
        ];
        enable = true;
      };
    };
  };
  blockNPM = pkgs.writers.writeBashBin "npm" ''
    cat << EOF

    #################################################
    #                                               #
    #   Take your npm elsewhere!!       (\/)        #
    #                                   (..)        #
    #                                   c(")(")     #
    #                                               #
    #   Easter bunny says: "Use yarn instead"       #
    #                                               #
    #################################################

    EOF
  '';
in
pkgs.mkShell ({
  buildInputs = [
    pkgs.elmPackages.elm
    nix-pre-commit-hooks.elm-format
    nix-pre-commit-hooks.prettier
    pkgs.elmPackages.elm-review
    pkgs.elmPackages.elm-language-server
    pkgs.elmPackages.elm-json
    pkgs.nodejs-14_x
    pkgs.yarn
    blockNPM
  ] ++ (if pkgs.system == "aarch64-darwin" then [ ] else [ pkgs.niv ]);


  shellHook = ''
    export PATH="${blockNPM}/bin:${toString ./node_modules/.bin}:$HOME/.npm-global/bin:$PATH"
    git config --unset-all core.hooksPath || true
    ${pre-commit-check.shellHook}
  '';

}) 


