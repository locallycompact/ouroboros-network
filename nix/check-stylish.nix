{ runCommand, git, gnugrep, stylish-haskell }:

runCommand "check-stylish"
{
  buildInputs = [ git gnugrep stylish-haskell ];
  src = ./..;
}
  ''
    unpackPhase
    cd $sourceRoot
    cp ./scripts/ci/check-stylish.sh ./scripts/ci/patchedCheckStylish.sh
    patchShebangs ./scripts/ci/patchedCheckStylish.sh
    ./scripts/ci/patchedCheckStylish.sh
  ''
