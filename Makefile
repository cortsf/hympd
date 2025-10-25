build:
	nix build
build-static:
	nix build .#x86_64-unknown-linux-musl:hympd:exe:hympd
run:
	nix develop -c ghcid "--command=cabal repl hympd:exe:hympd" -T Main.main --setup ":l Main Javascript Web Stream Utility" -s ':set args --port=3009'
