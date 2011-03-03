Plan: Scanner.hs Language.hs Plan.hs
	ghc --make -O3 Plan.hs

%.hs: %.x
	alex $^

%.hs: %.y
	happy $^
