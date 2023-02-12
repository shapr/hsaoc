{s}: rec
{
  ghcidScript = s "dev" "ghcid --command 'cabal new-repl lib:hsaoc' --allow-eval --warnings";
  allScripts = [ghcidScript];
}
