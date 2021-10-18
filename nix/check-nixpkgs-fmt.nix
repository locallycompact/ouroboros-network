{ runCommand, git, nixpkgs-fmt }:

runCommand "check-nixpkgs-fmt"
{
  buildInputs = [ git nixpkgs-fmt ];
  src = ./..;
}
  ''
    unpackPhase
    cd $sourceRoot
    cp ./scripts/ci/check-nixpkgs-fmt.sh ./scripts/ci/patchednixpkgsFmt.sh
    patchShebangs ./scripts/ci/patchednixpkgsFmt.sh
    ./scripts/ci/patchednixpkgsFmt.sh
  ''
