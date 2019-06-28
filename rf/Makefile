all: 	clean check dialyzer build

ci:		clean check build

clean:
	@rebar3 clean

check:
	@rebar3 eunit

dialyzer:
	@rebar3 dialyzer

build:
	@rebar3 escriptize
	@cp _build/default/bin/rf .
