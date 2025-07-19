run:
	ghcid "--command=cabal repl hympd:exe:hympd" -T Main.main --setup ":l Main Javascript Web Stream"
	# ghcid "--command=ghci app/Main.hs" -T main --setup ":l Javascript"
