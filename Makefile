REBAR := /usr/local/bin/rebar

.PHONY: all deps doc test clean release rel

all: deps
	$(REBAR) compile

rel:
	$(REBAR) generate

deps:
	$(REBAR) get-deps

doc:
	$(REBAR) doc skip_deps=true

test:
	$(REBAR) eunit skip_deps=true

clean:
	$(REBAR) clean

APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
	xmerl webtool snmp public_key mnesia eunit syntax_tools compiler
COMBO_PLT = $(HOME)/.maelstrom_combo_dialyzer_plt

check_plt: compile
	dialyzer --check_plt --plt $(COMBO_PLT) --apps $(APPS) \
		deps/*/ebin

build_plt: compile
	dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS) \
		deps/*/ebin

dialyzer: compile
	@dialyzer --plt $(COMBO_PLT) -Wno_return -c ebin
