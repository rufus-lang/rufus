all: 	check build

build:
	@rebar3 escriptize
	@cp _build/default/bin/rf .

check:
	@rebar3 do eunit
