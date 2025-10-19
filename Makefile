build:
	nix build
run:
	ghcid "--command=cabal repl hympd:exe:hympd" -T Main.main --setup ":l Main Javascript Web Stream Utility" -s ':set args --port=3009'
