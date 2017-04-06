SHELL = /bin/bash
.RECIPEPREFIX = >
git_repo_root ?= $(PWD)/..
local_bin_path ?= $(HOME)/.local/bin
resolver = "nightly-"`date "+%Y-%m-05"`

dev-all : int-test-basic

ci-all : check
> stack --install-ghc --resolver $(resolver) build --test --only-dependencies
> stack --resolver $(resolver) build --test --haddock --no-haddock-deps 
> stack --install-ghc --resolver lts-8 build --test --only-dependencies
> stack --resolver lts-8 build --test --haddock --no-haddock-deps
> stack --install-ghc --resolver lts-7 build --test --only-dependencies
> stack --resolver lts-7 build --test --haddock --no-haddock-deps
> stack --install-ghc --resolver lts-6 build --test --only-dependencies
> stack --resolver lts-6 build --test --haddock --no-haddock-deps
# lts3?, lts2?

install : check
> stack --resolver $(resolver) --local-bin-path="$(local_bin_path)" $(STACK_ARGS) build --copy-bins

int-test-full : check

int-test-basic : check

check : build
> stack --resolver $(resolver) build --test

build :
> stack init --force
> stack --install-ghc --resolver $(resolver) build --test --only-dependencies
> stack --resolver $(resolver) build --haddock --no-haddock-deps

