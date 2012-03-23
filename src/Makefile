.PHONY: doc

all:
	ghc --make Main.hs

caster:
	ghc --make Caster.hs

doc:
	haddock -o doc -h *.hs

clean:
	@rm -vrf `hg stat -u`
