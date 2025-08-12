.PHONY: hoogle build test repl serialise clean requires_nix_shell

hoogle: requires_nix_shell
	@hoogle server --local --port=8070 > /dev/null &

build: requires_nix_shell
	@cabal v2-build

test: requires_nix_shell
	@cabal v2-test --test-show-details=direct

repl: requires_nix_shell
	@nix develop -c cabal new-repl

serialise: requires_nix_shell
	@cabal run serialise

format: requires_nix_shell
	@",format"

lint: requires_nix_shell
	@",lint"

BUILD_PATH = "dist-newstyle"
clean:
	@[ ! -e $(BUILD_PATH) ] || rm -rf $(BUILD_PATH)

# # Target to use as dependency to fail if not inside nix-shell.
requires_nix_shell:
	@ [ "$(IN_NIX_SHELL)" ] || { \
	echo "The $(MAKECMDGOALS) target must be run from inside a nix shell"; \
	echo "    run 'nix develop .#onchain' first"; \
	false; \
	}
