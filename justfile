# Run stack to build, with file-watch and dynamic build flags.
build:
    stack $(stack_opts) build --ghc-options="-dynamic" --test --haddock --no-haddock-deps

alias b := build

# Run format pass, both simformat and code-style-checks, in nix so you don't get a pile of nonsense cabal changes.
format:
    fd .hs | xargs --max-args 1 --  fourmolu --mode inplace 

alias f := format

build-full:
    stack $(stack_opts) --stack-yaml=stack.8.8.4.yaml build --ghc-options="-dynamic" --test --haddock --no-haddock-deps
    stack $(stack_opts) --stack-yaml=stack.9.2.4.yaml build --ghc-options="-dynamic" --test --haddock --no-haddock-deps
