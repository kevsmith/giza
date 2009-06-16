PKGNAME=giza
VERSION=0.1.0
all: test

ebin:
	mkdir ebin

include:
	mkdir -p include
	cp src/giza.hrl include

compile: ebin include
	cd src;erl -make

test: compile
	cd t;erl -make
	prove t/*.t

integration: compile
	@echo !!! These tests require searchd to be running on localhost !!!
	cd t;erl -make
	prove t/*.integ

package: clean
	mkdir ebin
	@mkdir $(PKGNAME)-$(VERSION)/ && cp -rf ebin Makefile README.markdown src t $(PKGNAME)-$(VERSION)
	@COPYFILE_DISABLE=true tar zcf $(PKGNAME)-$(VERSION).tgz $(PKGNAME)-$(VERSION)
	@rm -rf $(PKGNAME)-$(VERSION)/


clean:
	rm -f *.tgz *.tar.gz
	rm -rf $(PKGNAME)-$(VERSION)
	rm -rf ebin
	rm -f t/*.beam
	rm -rf include
