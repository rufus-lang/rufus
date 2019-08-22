all: 	clean check dialyzer build

ci:		clean check prime-dialyzer dialyzer build

clean:
	@rebar3 clean

check:
	@rebar3 eunit

# Dialyzer emits many warnings about code outside the project the first time it
# runs. `prime-dialyzer` exists to warm the cache and ignore these warnings. A
# subsequent run of the `dialyzer` target will emit warnings related to the
# project.
prime-dialyzer:
	@-rebar3 dialyzer > /dev/null

dialyzer:
	@rebar3 dialyzer

build:
	@rebar3 escriptize
	@cp _build/default/bin/rf .
