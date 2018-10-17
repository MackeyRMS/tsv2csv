.RECIPEPREFIX = > 
SHELL := /bin/bash
git_repo_root ?= $(PWD)/..
local_bin_path ?= $(HOME)/.local/bin

.PHONY: install
install:
> stack install --local-bin-path="$(local_bin_path)"

.PHONY: clean
clean:
> stack clean --full

.PHONY: test
test:
> ./test.sh

.PHONY: docker
docker:
> mkdir -p dockerbin
> stack install --local-bin-path dockerbin --docker
