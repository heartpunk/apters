Plan: Scanner.hs Language.hs Plan.hs
	ghc --make -O3 Plan.hs

%.hs: %.x
	alex $^

# Note: Testing with Happy 1.18.4 and GHC 6.12.1, we can't use happy -g
# because GHC reports "Bindings containing unlifted types must use an
# outermost bang pattern". Without -g we can't use -c, and the
# documentation says we don't want to use -a without -g, so there are no
# usable optimization flags.
%.hs: %.y
	happy $^
