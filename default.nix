let
  nixos_20_09 = import <nixos_20_09> { };
  unstable = import <nixpkgs> { };
in unstable.stdenv.mkDerivation rec {
  name = "servant-gdp";
  buildInputs = [
    nixos_20_09.ormolu
    nixos_20_09.hlint
    nixos_20_09.nodePackages.prettier
    unstable.stack
    unstable.haskellPackages.haskell-language-server
    unstable.haskellPackages.hspec-discover
  ];
  env = unstable.buildEnv {
    name = name;
    paths = buildInputs;
  };
  shellHook = ''
    git config core.hooksPath .hooks/
  '';
  LANG = ""; # This is required to build the project dependencies
}
