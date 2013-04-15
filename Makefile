compile:
	./rebar compile

clean:
	./rebar clean

.PHONY: test

TESTS=""
test:
ifeq ($(TESTS), "")
	./rebar -j1 eunit
else
	./rebar -j1 eunit suite=$(TESTS)
endif

.PHONY: doc
doc:
	./rebar doc

EBINPATH=""

shell: compile
	erl -pa ebin $(EBINPATH) -s psycho_devmode
