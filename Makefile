make:
	mkdir bin obj
	gprbuild -PBuild

clean:
	rm -rf bin obj
