all:
	ghc --make formula-2-bdd.hs

zip:
	zip flp-fun-xsebek02.zip formula-2-bdd.hs Makefile README README.md tests/*
