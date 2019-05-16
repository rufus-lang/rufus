all: 	clean check build

clean:
	@rebar3 clean

check:
	@rebar3 eunit

build:
	@rebar3 escriptize
	@cp _build/default/bin/rf .
