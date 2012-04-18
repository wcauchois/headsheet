.PHONY: all

all: headsheet

headsheet: Main.hs
	ghc -o headsheet Main.hs
