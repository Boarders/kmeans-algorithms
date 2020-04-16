code-dirs = src

default:
	cabal run plot-clusters && xdg-open "gaussian.png"

hlint:
	hlint --no-exit-code $(code-dirs)

stylish:
	@find $(code-dirs) -type f -name "*.hs" | while read fname; do \
	  stylish-haskell -i "$$fname"; \
	done
