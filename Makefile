LIBDIR=`erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`
PKGNAME=giza
VERSION=0.2.0

all: test docs

ebin:
	mkdir ebin

include:
	mkdir -p include
	cp src/giza.hrl include

compile: ebin include
	cd src;erl -make
	cp src/giza.app ebin

install:
	mkdir -p $(prefix)/$(LIBDIR)/$(PKGNAME)-$(VERSION)/{ebin,include}
	for i in ebin/*.beam include/*.hrl; do install $$i $(prefix)/$(LIBDIR)/$(PKGNAME)-$(VERSION)/$$i ; done

test: compile
	cd tests;erl -make
	prove tests/*.t

integration: compile
	@echo !!! These tests require searchd to be running on localhost !!!
	cd tests;erl -make
	prove tests/*.integ

package: clean
	mkdir ebin
	@mkdir $(PKGNAME)-$(VERSION)/ && cp -rf ebin Makefile README.markdown src t $(PKGNAME)-$(VERSION)
	@COPYFILE_DISABLE=true tar zcf $(PKGNAME)-$(VERSION).tgz $(PKGNAME)-$(VERSION)
	@rm -rf $(PKGNAME)-$(VERSION)/

doc:
	mkdir -p doc

docs:	doc doc/*.html

doc/*.html:
	erl -eval 'edoc:files(["./src/giza_datetime.erl","./src/giza_query.erl","./src/giza_request.erl","./src/giza_response.erl","./src/giza_protocol.erl"])'\
	 -noshell -s init stop
	mv *.html erlang.png stylesheet.css edoc-info doc
clean:
	rm -f *.tgz *.tar.gz edoc-info *.html erlang.png erl_crash.dump
	rm -rf $(PKGNAME)-$(VERSION)
	rm -rf ebin
	rm -rf doc
	rm -f t/*.beam
	rm -rf include
