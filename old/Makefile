index.html: Main.hs template.html talks
	ghc --make Main.hs
	./Main

commit: Main.hs template.html talks index.html
	git add Main.hs template.html talks index.html
	git commit -m update
