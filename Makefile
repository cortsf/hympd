run:
	ghcid "--command=cabal repl hmpd:exe:hmpd" -T Main.main --setup ":l Main Javascript Web Stream Utility" -s ':set args --port=3009'
