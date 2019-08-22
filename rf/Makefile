all: 	clean check dialyzer build

ci:		clean check prime-dialyzer dialyzer build

clean:
	@rebar3 clean

check:
	@rebar3 eunit

prime-dialyzer:
	@-rebar3 dialyzer > /dev/null

dialyzer:
	@rebar3 dialyzer

build:
	@rebar3 escriptize
	@cp _build/default/bin/rf .
