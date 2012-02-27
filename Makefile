all:
	ghc --make Main.hs

caster:
	ghc --make Caster.hs

gl:
	ghc --make GL.hs

clean:
	@rm -vrf `hg stat -u`
