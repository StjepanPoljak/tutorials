proj=types

$(proj): $(proj).hs
	ghc -dynamic -Wname-shadowing $<

.PHONY=clean
clean:
	rm -rf *.hi *.ho $(proj)
