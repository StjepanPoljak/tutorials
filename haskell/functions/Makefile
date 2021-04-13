proj=functions

$(proj): $(proj).hs
	ghc -dynamic $<

.PHONY=clean
clean:
	rm -rf *.hi *.ho $(proj)
