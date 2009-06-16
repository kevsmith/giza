PKGNAME=giza
VERSION=0.1.0

all: test docs

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

doc:
	mkdir -p doc

docs:	doc doc/*.html

doc/*.html:
	erl -eval 'edoc:files(["./src/giza_datetime.erl","./src/giza_query.erl","./src/giza_request.erl",\
			       "./src/giza_response.erl","./src/giza_protocol.erl"])' -noshell -s init stop
	mv *.html erlang.png stylesheet.css edoc-info doc
clean:
	rm -f *.tgz *.tar.gz
	rm -f erl_crash.dump
	rm -rf $(PKGNAME)-$(VERSION)
	rm -rf ebin
	rm -rf doc
	rm -f t/*.beam
	rm -rf include
