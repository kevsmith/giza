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

package: compile
	mkdir -p /tmp/giza;cp -R * /tmp/giza;rm -rf /tmp/giza/.git /tmp/giza/tests;tar czf giza.tar.gz -C /tmp giza
clean:
	rm -f giza.tar.gz
	rm -rf ebin
	rm -f t/*.beam
	rm -rf include
