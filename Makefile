REBAR := /usr/local/bin/rebar

.PHONY: all deps doc test clean release

all: deps
	$(REBAR) compile

rel:
	cd rel && $(REBAR) generate

deps:
	$(REBAR) get-deps

doc:
	$(REBAR) doc skip_deps=true

test:
	$(REBAR) eunit skip_deps=true

clean:
	$(REBAR) clean

dialyze: all test
	dialyzer --src src/*.erl
